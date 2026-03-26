scaled_coef_polr <- function(ModelResults, data, subvar = FALSE, vars = NULL) {
  library(MASS)
  library(dplyr)
  require(ggplot2)
  library(gridExtra)
  library(tidyr)
  library(stringr)
  library(purrr)
  # paste the coef togeter
  modcoef = Map(function(x, y) c(coef(x), y$zeta), x = ModelResults, y = ModelResults)
  modelcoef <- lapply(modcoef, function(x) FUN =round(x, digits = 4))  
  #get the sd
  modelse = lapply(ModelResults, function(x) FUN = sqrt(diag(vcov(x))))
  #get the names together               
  varnames = Map(function(x, y) c(names(x$coefficients), names(y$zeta)), x = ModelResults, y = ModelResults)
  
  dfplot <- list()
  for (i in 1:length(modelcoef)){
    ad = 0.5 / modelse[[i]]
    modelcoef[[i]] = modelcoef[[i]]* ad
    modelse[[i]] = modelse[[i]]*ad 
    
    ylo95 =modelcoef[[i]] - qt(.975, nrow(data))*(modelse[[i]])
    yhi95 =modelcoef[[i]] + qt(.975, nrow(data))*(modelse[[i]])
    ylo90 =modelcoef[[i]] - qt(.95, nrow(data))*(modelse[[i]])
    yhi90 =modelcoef[[i]] + qt(.95, nrow(data))*(modelse[[i]])
    dfplot[[i]] = data.frame(varnames[[i]], modelcoef[[i]], modelse[[i]], ylo95, yhi95,
                             ylo90, yhi90,
                             Model = paste('模型',seq_along(modelcoef)[i]))
  }
  
  coefficientdata = do.call(rbind,dfplot)
  colnames(coefficientdata) <- c("Variables", "Coefficients", "S.E",
                                 "Low95CI", "High95CI", 
                                 "Low90CI", "High90CI",
                                 "Model.Name")
  df <- coefficientdata %>%
    dplyr::mutate(Variables = as.character(Variables)) %>%
    dplyr::mutate(Variables = ifelse(Variables == "1|2", "cut1", Variables)) %>% 
    dplyr::mutate(Variables = ifelse(Variables == "2|3", "cut2", Variables)) %>% 
    dplyr::mutate(Variables = ifelse(Variables == "3|4", "cut3", Variables)) %>% 
    dplyr::mutate(Variables = ifelse(Variables == "4|5", "cut4", Variables)) %>% 
    dplyr::arrange(Variables) 
  
  
  
  if(subvar == TRUE){
    df <- df[grep(paste(vars, collapse = "|"), df$Variables),]
    #re-order the variables: if factor, the order of factor levels is used; if character, an alphabetical order ist used
    df$Variables <- factor(df$Variables, levels = vars)
  } 
  
  p = ggplot(df, aes(colour = Model.Name)) + 
    geom_hline(yintercept = 0, lty = 2) +
   # geom_linerange(aes(x = Variables, ymin = Low90CI,
    #                   ymax = High90CI),
     #              lwd = 2, position = position_dodge(width = 1/2)) +
    geom_pointrange(aes(x = Variables, y = Coefficients, ymin = Low95CI,
                        ymax = High95CI, shape = Model.Name),
                    lwd = 1, position = position_dodge(width = 1/2),
                    fill = "WHITE") +
    coord_flip() + 
    theme_bw() + xlab("") + ylab("标准化回归系数") + 
    theme(legend.position="bottom",
          legend.title=element_blank(),
          axis.text = element_text(size=14),
          text = element_text(size=14,family ='STSong'),
          plot.title = element_text(hjust = .5, size = 16, face = "bold"))
  
  return(p)   
} 




polr_fd <- function(ModelResult, n_sim = 1000, varible){
  
  library(MASS)
  library(arm)
  set.seed(1234)
  sim_coef <- sim(ModelResult, n.sims = n_sim)
  
  
  ## get a the design matrix from the model
  
  X0 <- X1 <- model.matrix(ModelResult)
  # get rid of intercept
  X0 <- X0[ ,2:ncol(X0)] 
  X1 <- X1[ ,2:ncol(X1)] 
  
  # set the simulation scenario
  X0[,varible] <- 0
  X1[,varible] <- 1
  
  n_lev <- length(ModelResult$lev)
  fd <- array(NA, c(n_sim, n_lev))
  
  
  ## get the predicted probabilities 
  
  p1_h <- apply(X1, 1, function (x) plogis(coef(sim_coef)[,1] -  coef(sim_coef)[,n_lev:ncol(coef(sim_coef))] %*% x))
  p1_l <- apply(X0, 1, function (x) plogis(coef(sim_coef)[,1] -  coef(sim_coef)[,n_lev:ncol(coef(sim_coef))] %*% x))
  
  p2_h <-  apply(X1, 1, function (x) plogis(coef(sim_coef)[,2] -  coef(sim_coef)[,4:ncol(coef(sim_coef))] %*% x)) - apply(X1, 1, function (x) plogis(coef(sim_coef)[,1] -  coef(sim_coef)[,4:ncol(coef(sim_coef))] %*% x))
  p2_l <-  apply(X0, 1, function (x) plogis(coef(sim_coef)[,2] -  coef(sim_coef)[,4:ncol(coef(sim_coef))] %*% x)) - apply(X0, 1, function (x) plogis(coef(sim_coef)[,1] -  coef(sim_coef)[,4:ncol(coef(sim_coef))] %*% x))
  
  
  p3_h <-  apply(X1, 1, function (x) plogis(coef(sim_coef)[,3] -  coef(sim_coef)[,4:ncol(coef(sim_coef))] %*% x)) - apply(X1, 1, function (x) plogis(coef(sim_coef)[,2] -  coef(sim_coef)[,4:ncol(coef(sim_coef))] %*% x))
  p3_l <-  apply(X0, 1, function (x) plogis(coef(sim_coef)[,3] -  coef(sim_coef)[,4:ncol(coef(sim_coef))] %*% x)) - apply(X0, 1, function (x) plogis(coef(sim_coef)[,2] -  coef(sim_coef)[,4:ncol(coef(sim_coef))] %*% x))
  
  p4_h <- 1 -  apply(X1, 1, function (x) plogis(coef(sim_coef)[,3] -  coef(sim_coef)[,4:ncol(coef(sim_coef))] %*% x)) 
  p4_l <- 1 -  apply(X0, 1, function (x) plogis(coef(sim_coef)[,3] -  coef(sim_coef)[,4:ncol(coef(sim_coef))] %*% x)) 
  
  ## get the first different
  fd[, 1] <- apply(p1_h - p1_l, 1, mean)
  fd[, 2]<- apply(p2_h - p2_l, 1, mean)
  fd[, 3]<- apply(p3_h - p3_l, 1, mean)
  fd[, 4] <- apply(p4_h - p4_l, 1, mean)
  
  fd_data <- as.data.frame(fd)
  
  return(fd_data)
}


## functions
scaled_coef_cluster <- function(ModelResults, data, clusterid, subvar = FALSE, vars = NULL) {
  library(multiwayvcov)
  library(lmtest)
  library(dplyr)
  require(ggplot2)
  library(gridExtra)
  library(tidyr)
  library(stringr)
  library(purrr)
  
  cluster <- data[, clusterid]
  
  vcov_cluster = lapply(ModelResults, function(x) FUN = cluster.vcov(x, 
                                                                     cluster))
  modcoef = Map(function(x, y) coeftest(x, y), x = ModelResults, y = vcov_cluster)
  modcoef <- lapply(modcoef, function(x) FUN =round(x, digits = 4))  
  modelcoef = lapply(modcoef, function(x) FUN = x[, "Estimate"])
  modelse = lapply(modcoef, function(x) FUN = x[, "Std. Error"])
  varnames = lapply(ModelResults, function(x) FUN = names(x$coefficients))
  dfplot <- list()
  for (i in 1:length(modelcoef)){
    ad = 0.5 / modelse[[i]]
    modelcoef[[i]] = modelcoef[[i]]* ad
    modelse[[i]] = modelse[[i]]*ad 
    
    ylo95 =modelcoef[[i]] - qt(.975, nrow(data))*(modelse[[i]])
    yhi95 =modelcoef[[i]] + qt(.975, nrow(data))*(modelse[[i]])
    ylo90 =modelcoef[[i]] - qt(.95, nrow(data))*(modelse[[i]])
    yhi90 =modelcoef[[i]] + qt(.95, nrow(data))*(modelse[[i]])
    dfplot[[i]] = data.frame(varnames[[i]], modelcoef[[i]], modelse[[i]], ylo95, yhi95,
                             ylo90, yhi90,
                             Model = paste('Model',seq_along(modelcoef)[i]))
  }
  
  coefficientdata = do.call(rbind,dfplot)
  colnames(coefficientdata) <- c("Variables", "Coefficients", "S.E",
                                 "Low95CI", "High95CI", 
                                 "Low90CI", "High90CI",
                                 "Model.Name")
  df <- coefficientdata %>%
    dplyr::mutate(Variables = as.character(Variables)) %>%
    dplyr::arrange(Variables)
  
  if(subvar == TRUE){
    df <- df[grep(paste(vars, collapse = "|"), df$Variables),]
    #re-order the variables: if factor, the order of factor levels is used; if character, an alphabetical order ist used
    df$Variables <- factor(df$Variables, levels = vars)
  } 
  
  return(df)
  }
  

