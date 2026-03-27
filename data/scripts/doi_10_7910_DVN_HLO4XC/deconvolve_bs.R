# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code produces boostrap estimates of the standard error for the
# concentration metrics reported in Figure 7.

# It must be run in the top level of the replication archive directory.
# *********************************************************************

# Load library
library(foreign)
library(tidyverse)
library(devtools)
library(qvalue)
library(ggtext)
library(ggplot2)
library(gtools)
library(deconvolveR)
library(ExtDist)
library(Matrix)
library(numDeriv)
library(doParallel)
registerDoParallel(cores=10)

# Loop over genders
charlist <- c("white", "male")
for (tostudy in charlist) {
  tune_reg <- TRUE
  support_rest <- TRUE
  pd <- 5
  if (tostudy == "white") {
    if (!balanced) {
      reg <- 0.1938036
    } else {
      reg <- 0.1363966
    }
    divider <- 1/40
    xlab <- "Firm white-black contact rate gap"
    ylab <- "Posterior mean white-Black contact rate gap"
    leg_pos <- c(0.8, 0.9)
    xlb <- -0.08
    xub <- 0.15
  } else {
    if (!balanced) {
      reg <- 0.1447446
    } else {
      reg <- 0.2363372
    }
    divider <- 1/35
    xlab <- "Firm male-female contact rate gap"
    ylab <- "Posterior mean male-female contact rate gap"
    leg_pos <- c(0.2, 0.9)
    xlb <- -0.2
    xub <- 0.15
  }

  # Load up the data
  df <- read_dta(file='data/data.dta')

  # Drop apps sent out of order (~170 total)
  df <- df %>% group_by(job_id,pair) %>% 
    mutate(napps = n(),
           nblack = sum(black)) %>% ungroup()
  df <- df %>% filter(napps != 2 | nblack == 1)

  # Simple difference in means se and p-values with correct DOF
  means <- df %>% group_by(firm_id, job_id, .data[[tostudy]]) %>% summarize(mean_cb=mean(cb))
  means <- means %>% spread(.data[[tostudy]],mean_cb) %>% drop_na()

  ttester <- function(fid) {
    tmp <- means %>% filter(firm_id == fid)
    test <- t.test(tmp[["1"]],tmp[["0"]], paired=TRUE)
    p <- test$p.value
    se <- test$stderr
    est <- test$estimate
    p_greater <- t.test(tmp[["1"]],tmp[["0"]], paired=TRUE, alternative="greater")$p.value
    p_less <- t.test(tmp[["1"]],tmp[["0"]], paired=TRUE, alternative="less")$p.value
    return(cbind(firm_id=fid, est=est, se=se, ttest_p=p, ttest_p_greater=p_greater, ttest_p_less=p_less))
  }

  firm_effects_orig <- as.data.frame(do.call(rbind,lapply(unique(df$firm_id),FUN=ttester)))

  # Efron density estimation parameters
  width <- .0001
  ntaus <- 1000

  # Set up objects
  firm_effects_orig$t_stat <- firm_effects_orig$est / firm_effects_orig$se
  zf <- firm_effects_orig$est / firm_effects_orig$se
  ses <- firm_effects_orig$se

  if (tostudy == "white") {
    if (support_rest == TRUE) {
      leftlim <-  0
    } else {
      leftlim <- min(zf)-0.5
    }
    rightlim <- max(zf)+0.5
    betax <- seq(-0.05,max(zf)*max(firm_effects_orig$se),by=width)
    blacklim <- c(-0.05,0.08)
  } else {
    leftlim <-  min(zf)-0.5
    rightlim <- max(zf)+0.5
    betax <- seq(min(zf)*max(firm_effects_orig$se),max(zf)*max(firm_effects_orig$se),by=width)
    blacklim <- c(-0.08,0.08)
  }
  tauwidth = (rightlim - leftlim)/ntaus
  mus <- seq(leftlim, rightlim, by=tauwidth)

  # Function to put through loop
  estimate_shares <- function(firm_effects) {
    first = mean(firm_effects$est)
    second = (1/(nrow(firm_effects))*sum((firm_effects$est - first)**2) -
                (nrow(firm_effects)-1)/(nrow(firm_effects)**2)*sum(firm_effects$se**2))
    
    # Set up objects
    firm_effects$t_stat <- firm_effects$est / firm_effects$se
    zf <- firm_effects$est / firm_effects$se
    ses <- firm_effects$se

    # Generate beta density
    deltadens <- function(delta, mudens) {
      support <- delta/firm_effects$se
      fz <- sapply(support, function(x) {
        if (x < min(mus)) {
          return(0) # assume density is zero outside support for mus
        }
        else if (x > max(mus)) {
          return(0) # assume density is zero outside support for mus
        }
        return(mudens[which(abs(mus-x)==min(abs(mus-x)))])
      })
      inside <- fz/firm_effects$se
      dens <- sum(inside)/length(firm_effects$se)
      return(dens)
    }
    
    # Return G
    get_g <- function(c0) {
      res <- deconv(tau=mus, X=firm_effects$est/firm_effects$se, family="Normal",
                    c0=c0, pDegree=pd) 
      mudens <- res$stats[, "g"]
      betay <- sapply(betax, deltadens, mudens=mudens)
      return(list(res=res,betay=betay))
    }
    
    # Tune Efron regularization parameter
    if (tune_reg) {
      find_reg = function(c0) {
        betay <- get_g(c0)$betay
        implied_sd <- (sum(betax**2*betay)/sum(betay)-
                         (sum(betax*betay)/sum(betay))**2)
        gap = (implied_sd - second)**2*1000
        gap
      }
      o = optimize(find_reg, interval=c(0, .8))
      reg = o$minimum[[1]]
    }
    
    # Efron density
    res <- get_g(reg)
    betay <- res$betay
    betay_sum <- sum(betay)
    betay <- betay / betay_sum / width # make beta density integrate to 1
    implied_mean <- sum(betax*betay)/sum(betay)
    implied_var <- (sum(betax**2*betay)/sum(betay)-
                      (sum(betax*betay)/sum(betay))**2)
    implied_sd <- implied_var**0.5
    
    # Discrete density of mus
    result <- res$res
    mudens <- result$stats[, "g"] / tauwidth
    mus <- result$stats[, "theta"]
    
    # Tope share calculation
    calc_topshare <- function(betay) {
      dens <- data.frame(betax,betay)
      dens$betax <- abs(dens$betax)
      dens <- dens[order(dens$betax),]
      cdf <- cumsum(dens$betay) / sum(dens$betay)
      lower_lim <- min(dens$betax[cdf >= 0.8])
      share <- sum(dens$betax*dens$betay*(dens$betax >= lower_lim)) / sum(dens$betax*dens$betay)
      return(share)
    }
    
    # Gini calculation
    calc_gini <- function(betay) {
      dens <- data.frame(betax,betay)
      dens$betax <- abs(dens$betax)
      dens <- dens[order(dens$betax),]
      yield  <- cumsum(abs(dens$betax)*dens$betay/sum(dens$betay))
      under <- sum(yield*dens$betay/max(yield))/sum(dens$betay)
      return(1-2*under)
    }
    
    topshare <- calc_topshare(betay)
    gini <- calc_gini(betay)
    return(list(gini=gini,topshare=topshare,betay=betay))
  }

  # Boostrap resampples
  nstraps <- 1000
  set.seed(7923458)
  rdraws <- mapply(function(x,y) rnorm(nstraps,x,y), firm_effects_orig$est, firm_effects_orig$se)

  # Complete function
  bsfunc <- function(k) {
    # Resample firm effects
    firm_effects <- firm_effects_orig
    firm_effects['est'] <- rdraws[k,]
    
    # Estimate functionals
    ans <- estimate_shares(firm_effects)
    return(ans)
  }

  # Run bootstraps in parallel
  x <- foreach(k=1:nstraps) %dopar% bsfunc(k)
  ginis <- rep(0,nstraps)
  topshares <- rep(0,nstraps)
  betays <- list()
  for (i in 1:nstraps) {
    ginis[i] <- x[[i]]$gini
    topshares[i] <- x[[i]]$topshare
    betays[[i]] <- as.vector(x[[i]]$betay)
  }
  betays <- do.call(cbind, betays)
  lq <- apply(betays, 1, function(x) quantile(x, probs=0.025))
  uq <- apply(betays, 1, function(x) quantile(x, probs=0.975))

  # Report SES
  gini_sd <- sd(ginis)
  topshare_sd <- sd(topshares)
  truth <- estimate_shares(firm_effects_orig)
  print(paste("Standard errors for",tostudy))
  print(paste("Gini:",truth$gini,"sd:",gini_sd))
  print(paste("Topshare:",truth$topshare,"sd:",topshare_sd))
}

