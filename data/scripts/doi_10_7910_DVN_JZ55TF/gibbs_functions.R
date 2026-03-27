################################################
## Function to update strata probabilities, p ##
################################################

update.p <- function(tr, z, d, omega, g, possible.strata) {
  
  ## create a PO for D strata grid and subset to possible strata
  ## this will help us map PO strata to observed strata
  strata_grid <- expand.grid(d11 = c(1, 0), d01 = c(1, 0), d0 = c(1, 0))
  strata_labs <- paste0("s", do.call(paste0, strata_grid))
  rownames(strata_grid) <- strata_labs
  strata_grid <- strata_grid[possible.strata, ]
  
  tzd_grid <- expand.grid(tr = c(1, 0), z = c(1, 0), d = c(1, 0))
  tzd_grid_str <- do.call(paste0, tzd_grid)
  tzd_data_str <- paste0(tr, z, d)
  n_levs <- nrow(tzd_grid)
  
  ## maps each TZD strata to the column of strata_grid corresponding
  ## to the PO of D observed in that strata. Allows us to find the
  ## "compatible strata" below
  ps_obs_crosswalk <- case_when(
    tzd_grid$tr == 1 & tzd_grid$z == 1 ~ 1,
    tzd_grid$tr == 0 & tzd_grid$z == 1 ~ 2,
    tzd_grid$z == 0 ~ 3
  )
  p <- matrix(0, nrow = length(tr), ncol = length(possible.strata))
  colnames(p) <- possible.strata
  for (k in 1:n_levs) {
    k_units <- tzd_data_str == tzd_grid_str[k]
    ## which PO for D do we observe in this strata of TZD?
    which_d_po <- ps_obs_crosswalk[k]
    ## which PO strata are compatible with this level?
    compat_strata <- which(strata_grid[, which_d_po] == tzd_grid$d[k])
    ## calculate numerators on the log scale
    num <- omega[k_units, compat_strata, drop = FALSE] +
      g[k_units, compat_strata, drop = FALSE]
    ## logsumexp trick to avoid underflows
    ## see eg. https://blog.feedly.com/tricks-of-the-trade-logsumexp/
    num_max <- apply(num, 1, max)
    den <- log(rowSums(exp(num - num_max))) + num_max
    ## covert back to regular scale
    p[k_units, compat_strata] <- exp(as.matrix(num) - den)
  }
  return(as.data.frame(p))
}


##################################################
## Function to update strata proportions, omega ##
##################################################

update.omega <- function(covars, psis) {
  num <- covars %*% psis
  num_max <- apply(num, 1, max)
  den <- log(rowSums(exp(num - num_max))) + num_max
  omega.mlogit <- num - den
  colnames(omega.mlogit) <- colnames(psis)
  omega.mlogit <- as.data.frame(omega.mlogit)
  return(omega.mlogit)
}

###############################################
## Function to get predicted probability, mu ##
###############################################

# covs<-covars[2,]
# s111 <- 1
# s100 <- 0
# t <- 1
# z <- 1
# current.betas <- betas



get.mu <- function(strata, t, z, covars, current.betas, possible.strata, type = "mu") {
  
  ## if given length 1 for any variable, make it compatible
  if (length(t) == 1) t <- rep(t, nrow(covars))
  if (length(z) == 1) z <- rep(z, nrow(covars))
  if (length(strata) == 1) strata <- rep(strata, nrow(covars))
  
  ## create strata indicator matrix
  used.strata <- possible.strata[possible.strata != "s000"]
  st.mat <- create.indicators(strata, used.strata)
  
  ## creat tz matrix
  tz.mat <- cbind(t = t, z = z, "t:z" = t * z)
  
  ## create t * strata matrix
  st.int.mat <- t * st.mat
  colnames(st.int.mat) <- paste0(colnames(st.mat), "*t")
  
  if('z:s111'%in%names(current.betas)){
    ## create z * strata matrix
    st.int.mat.z <- z * st.mat
    colnames(st.int.mat.z) <- paste0(colnames(st.mat), "*z")
    
    ## create t * strata matrix
    st.int.mat.z.t <- t * z * st.mat
    colnames(st.int.mat.z.t) <- paste0(colnames(st.mat), "*t*z")
    
    
  }
  
  ## combine into one design matrix
  if('z:s111'%in%names(current.betas)|'s111*t*z'%in%names(current.betas)){
    des.mat <- cbind(covars, tz.mat, st.mat, st.int.mat, st.int.mat.z, st.int.mat.z.t)
  } else{
    des.mat <- cbind(covars, tz.mat, st.mat, st.int.mat)
  }
  pred <- as.numeric(des.mat %*% current.betas)

  if (type == "mu") {
    mu.logit <- plogis(pred)
    mu.logit <- ifelse(is.na(mu.logit), 0, mu.logit)
    mu.logit <- unlist(mu.logit)
  } else if (type == "g"){
    mu.logit <- pred
  }
  return(mu.logit)
}



create.indicators <- function(x, vals) {
  ind.list <- lapply(vals, function(i) as.integer(x == i))
  out <- do.call(cbind, ind.list)
  colnames(out) <- vals
  return(out)
}
