################################################################################
## Simulate new data via bootstrap and predictions of outcome and response
################################################################################
library(multical)
library(glmnet)
library(ranger)
library(gbm)
library(tidyverse)
library(parallel)
library(Matrix)


#' @param X Covariate data
#' @param y_model Expected outcome
#' @param p_model Response probability
#' @param n Sample size, default is the same as X
draw_data <- function(X, y_model, pscore, n_dat = NULL, ...) {

  if(is.null(n_dat)) {
    n_dat <- nrow(X)
  }
  idxs <- sample.int(nrow(X), n_dat, replace = T)

  new_X <- X[idxs, , drop = F]
  new_y_model <- y_model[idxs]
  new_pscore <- pscore[idxs]

  new_y <- sapply(new_y_model,
                  function(p) sample(c(0, 1), 1, prob = c(1 - p, p)))
  new_resp <- sapply(new_pscore,
                  function(p) sample(c(0, 1), 1, prob = c(1 - p, p)))

  return(list(X = new_X, y = new_y, resp = new_resp,
              y_model = new_y_model, pscore = new_pscore))
}

#' Fit methods on simulated data
#' @param max_order Maximum order of interactions to balance
fit_methods <- function(sim_out, max_order = 4, ...) {

  X <- sim_out$X
  resp <- sim_out$resp
  y <- sim_out$y
  pscore <- sim_out$pscore
  y_model <- sim_out$y_model
  # rake, then higher order terms
  X_resp_df <- data.frame(cbind(resp, X, 1))

  names(X_resp_df) <- c("resp", paste0("X", 1:ncol(X)), "incol")


  bal_res <- lapply(1:max_order, multical,
                    formula = as.formula(paste("resp ~ ",
                                         paste(paste0("X", 1:ncol(X),
                                               collapse = "+")))),
        target_count = incol, data = X_resp_df, lambda = 1)
  

  weights <- bind_cols(lapply(bal_res, `[`, "weight"))
  names(weights) <- paste0("weights_order_", 1:max_order)

  cells <- bal_res[[1]] %>% select(-weight, -sample_count, -target_count, -lambda)
  colnames(cells) <- colnames(X)
  ridge_os <- c(3)
  
  fits <- lapply(ridge_os,
    function(o) {
      if(o != 1) {
      form <- as.formula(paste("~ . ^", o)) 
    } else {
      form <- ~ .
    }
      X_int <- Matrix::sparse.model.matrix(form, X)
      cv.glmnet(X_int[resp == 1,, drop = F], y[resp == 1],
                family = "binomial", alpha = 0)
    })

  # fit RF
  rf <- ranger(x = X[resp == 1,, drop = F], y = as.factor(y[resp == 1]),
               probability = T)
  
  # get predictions
  preds <- lapply(1:length(ridge_os), function(i) {
    o <- ridge_os[i]
    if(o != 1) {
      form <- as.formula(paste("~ . ^", o)) 
    } else {
      form <- ~ .
    }

    cells_int <- Matrix::sparse.model.matrix(form, cells)
    c(predict(fits[[i]],  cells_int, type = "response", s = "lambda.min"))
  }) %>% bind_cols()

  names(preds) <- paste0("out_ridge_order_", ridge_os)
  
  preds <- bind_cols(preds,
                     data.frame("out_rf" =  predict(rf, cells)$predictions[,2]))

  res <- bind_cols(
    cells,
    bal_res[[1]] %>% select(sample_count, target_count),
    weights,
    preds,
    ) %>%
    right_join(cbind(X, resp, y, pscore, y_model))
  return(res)
}

#' Summarize results from methods on a simulation run
get_results <- function(methods_res) {

  methods_res %>%
    summarise(naive = mean(y[resp == 1]),
              truth = mean(y),
              oracle = sum((y / pscore)[resp == 1]) / length(y),
              oracle_ht = sum((y / pscore)[resp == 1]) /
                              sum(1 / pscore[resp == 1]),
              oracle_adj = mean(y_model[resp == 0]) +
                            sum(((y - y_model) / pscore)[resp == 1]) / 
                            sum(1 / pscore[resp == 1])) -> oracles

  methods_res %>%
    pivot_longer(contains("weights_order"),
                 names_to = "weight_name", values_to = "weight") %>%
    pivot_longer(contains("out_"),
                 names_to = "out_name", values_to = "pred")  %>%
    group_by(weight_name, out_name) %>%
    summarise(weight_est = sum((weight * y)[resp == 1]) / sum(weight[resp == 1]),
              mr_est = mean(pred),
              drp_est = mr_est + sum((weight * (y - pred))[resp == 1]) /
                                    sum(weight[resp == 1]),
              weight_se2 = sum((weight * (y - weight_est))[resp == 1]^2) /
                        sum(weight[resp == 1])^2,
              mr_se2 = sum((y - pred)[resp == 1]^2) /
                        sum(resp == 1)^2,
              drp_se2 = sum((weight * (y - pred))[resp == 1]^2) /
                        sum(weight[resp == 1])^2) %>%
    ungroup() %>%
    bind_cols(oracles)
}


#' Generate data and estimate ATT
#' @param sim_num Current simulation number
#' @param ... Simulation arguments
single_run <- function(sim_num, ...) {

    cat("." )
    if(!(sim_num %% 100)) {
        cat("\n Simulation number", sim_num, ": ")
    }
    ## simulate data
    output <- draw_data(...)
    
    ## evaluate methods
    suppressMessages(results <- get_results(fit_methods(output, ...)))
    results$sim_num <- sim_num
    ## return the results
    return(results)
}



#' Run many simulations in parallel
#' @param n_sims Number of simulations to run
#' @param n_cores Number of cores
#' @param ... Simulation arguments
eval_general <- function(n_sims, n_cores=1, ...) {


    cat("Number of simulations", n_sims, "\n")
    out <- mclapply(
        1:n_sims,
        function(j) single_run(j, ...),
        mc.cores=n_cores)

    out <- out[lapply(out, function(x) !is.null(names(x))) == TRUE]
    cat("\n\n")
    return(bind_rows(out))
}
