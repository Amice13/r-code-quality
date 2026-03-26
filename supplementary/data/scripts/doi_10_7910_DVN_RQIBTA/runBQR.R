# load required packages

library("rstan")
library("parallel")
library("dplyr")
library("lubridate")
library("tibble")

# set random seed

set.seed(1999)

# set number of MCMC chains

nc <- 4
Sys.setenv("MC_CORES" = nc)

# compile Bayesian Quantile Regression (BQR) model

bayesian.model <- stan_model("./BQR.stan")

# read tide gauge metadata

gesla.meta <- read.csv("./GESLA.csv")

# initialize vector to store variance explained from PCA

var.exp <- numeric(length = nrow(gesla.meta))

# loop over tide gauge stations

for (tide.num in 1:nrow(gesla.meta)) {
  
  # read data (response/explanatory) for tide gauge station
  
  data <- read.csv(paste0("./bqr-data/", gesla.meta[["FILE.NAME"]][tide.num], ".csv"))
  
  # get years in record
  
  year <- unique(year(data[["Date"]]))
  
  # split to calibration/validation subsets
  
  cali.year <- sample(year, 27, replace = FALSE)
  vali.year <- setdiff(year, cali.year)
  
  cali.data <- data %>% filter(year(Date) %in% cali.year)
  vali.data <- data %>% filter(year(Date) %in% vali.year)
  
  # scale explanatory data to their Z-score
  
  for (i in 3:ncol(vali.data)) {
    vali.data[, i] <- (vali.data[, i] - mean(cali.data[, i])) / sd(cali.data[, i])
  }
  
  for (i in 3:ncol(cali.data)) {
    cali.data[, i] <- (cali.data[, i] - mean(cali.data[, i])) / sd(cali.data[, i])
  }
  
  # perform PCA
  
  cali.pca <- prcomp(cali.data[, 3:ncol(cali.data)], center = FALSE, scale. = FALSE)
  summ <- summary(cali.pca)
  rotation <- as.data.frame(cali.pca[["rotation"]])
  var.exp[tide.num] <- summ[["importance"]][3, 5]
  
  xcal <- cali.pca[["x"]][, 1:5]
  k <- dim(xcal)[2]
  
  xval <- (as.matrix(vali.data[, 3:ncol(vali.data)]) %*% cali.pca[["rotation"]])[, 1:5]
  
  # save calibration subset
  
  dir.create(paste0("./bqr-results/", gesla.meta[["FILE.NAME"]][tide.num]))
  write.csv(
    cbind(cali.data, xcal),
    paste0("./bqr-results/", gesla.meta[["FILE.NAME"]][tide.num], "/", "calibration-data.csv"),
    row.names = FALSE
  )
  
  # save loadings from PCA
  
  write.csv(rotation,
            paste0("./bqr-results/", gesla.meta[["FILE.NAME"]][tide.num], "/", "rotation.csv"),
            row.names = FALSE)
  
  # set percentile of interest for BQR
  
  tau = 0.90 
  
  # prepare list with Stan data inputs
  
  feed.data <-
    list(
      N = nrow(cali.data),
      k = k,
      x = xcal,
      y = cali.data[["Maximum.Surge"]],
      p = tau
    )
  
  # sample from the posterior distribution
  
  fit.model <-
    sampling(
      object = bayesian.model,
      data = feed.data,
      chains = nc,
      cores = nc,
      iter = 1000,
      warmup = 500,
      seed = 1999,
      control = list(adapt_delta = 0.97, max_treedepth = 12),
      sample_file = "Chain.txt",
      verbose = TRUE,
      show_messages = TRUE
    )
  
  # save summary of Bayesian results
  
  results <- summary(fit.model)$summary
  results <- as.data.frame(results)
  results <- results[-nrow(results), ]
  write.csv(
    results,
    paste0(
      "./bqr-results/",
      gesla.meta[["FILE.NAME"]][tide.num],
      "/",
      gsub("\\.", "", as.character(tau)),
      "-mcmc-results.csv"
    ),
    row.names = FALSE
  )
  
  # save MCMC iterations
  
  comb.chain <-
    extract(
      fit.model,
      inc_warmup = FALSE,
      include = TRUE,
      permuted = TRUE
    )
  comb.chain <- do.call(cbind.data.frame, comb.chain)
  comb.chain <- comb.chain[, -ncol(comb.chain)]
  write.csv(
    comb.chain,
    paste0(
      "./bqr-results/",
      gesla.meta[["FILE.NAME"]][tide.num],
      "/",
      gsub("\\.", "", as.character(tau)),
      "-comb-chain.csv"
    ),
    row.names = FALSE
  )
  
  # predict storm surge on validation subset
  
  bqr.coef <- results[["mean"]]
  
  xval <- cbind(rep(1, nrow(xval)), xval)
  colnames(xval)[1] <- "Intercept"
  
  vali.data[[paste0("Maximum.Surge.Prediction.", gsub("\\.", "", as.character(tau)))]] <- NA
  for (i in 1:nrow(vali.data)) {
    vali.data[[paste0("Maximum.Surge.Prediction.",
                      gsub("\\.", "", as.character(tau)))]][i] <- sum(bqr.coef * xval[i, ])
  }
  
  # save validation subset
  
  write.csv(
    cbind(vali.data, xval),
    paste0(
      "./bqr-results/",
      gesla.meta[["FILE.NAME"]][tide.num],
      "/",
      gsub("\\.", "", as.character(tau)),
      "-validation-data.csv"
    ),
    row.names = FALSE
  )
  
}