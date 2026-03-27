# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code produce deconvolution estimates of population contact gaps
# allowing for dependence of estimates on standard errors by splitting 
# firms into quantiles of their standard errors.

# It must be run in the top level of the replication archive directory.
# *********************************************************************

# Load library
library(haven)
library(tidyverse)
library(qvalue)
library(ggtext)
library(ggplot2)
library(gtools)
library(deconvolveR)
library(ExtDist)
library(Matrix)
library(numDeriv)
library(PoissonBinomial)
library(scales)
library(miceadds)
library(cowplot)

# Difference to study
charlist <- c("white", "male")
for (tostudy in charlist) {
  pd <- 5
  ngroups <- 2
  if (tostudy == "white") {
    divider <- 1/40
    xlab <- "Firm white-black contact rate gap"
    ylab <- "Posterior mean white-Black contact rate gap"
    leg_pos <- c(0.8, 0.9)
    xlb <- -0.08
    xub <- 0.15
  } else {
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

  firm_effects <- as.data.frame(do.call(rbind,lapply(unique(df$firm_id),FUN=ttester)))
  firm_effects <- means %>% group_by(firm_id) %>% summarize(white_mean = mean(`0`)) %>%
                  right_join(firm_effects, by='firm_id')
  firm_effects$zf <- firm_effects$est / firm_effects$se
  first = mean(firm_effects$est)
  second = (1/(nrow(firm_effects))*sum((firm_effects$est - first)**2) -
              (nrow(firm_effects)-1)/(nrow(firm_effects)**2)*sum(firm_effects$se**2))

  # Split into quartiles based on standard errors
  firm_effects <- firm_effects %>% arrange(se) %>% mutate(seq = ntile(se, ngroups))
  split_ests <- split(firm_effects, f=firm_effects$seq) 

  # Efron parameters
  width <- .0001
  ntaus <- 1000

  if (tostudy == "white") {
    leftlim <-  0
    rightlim <- max(firm_effects$zf)+0.5
    betax <- seq(-0.05, max(firm_effects$zf)*max(firm_effects$se),by=width)
    blacklim <- c(-0.05,0.08)
  } else {
    leftlim <-  min(firm_effects$zf)-0.5
    rightlim <- max(firm_effects$zf)+0.5
    betax <- seq(min(firm_effects$zf)*max(firm_effects$se),max(firm_effects$zf)*max(firm_effects$se),by=width)
    blacklim <- c(-0.08,0.08)
  }
  tauwidth = (rightlim - leftlim)/ntaus
  mus <- seq(leftlim, rightlim, by=tauwidth)

  # Run within each split
  get_g <- function(c0) {
    betay <- rep(0, length(betax))
    mudens_list <- list(rep(NA, ngroups))
    betay_list <- list(rep(NA, ngroups))
    ll <- 0
    for (g in 1:ngroups) {
      ests <- split_ests[[g]]
      res <- deconv(tau=mus, X=ests$est/ests$se, family="Normal",
                       c0=c0, pDegree=pd) 
      ll <- ll + res$loglik(res$mle)
      
      # Add to density of mus
      mudens <- res$stats[, "g"]
      mudens_list[[g]] <- mudens
      
      # Add to density of betas
      deltadens <- function(delta) {
        support <- delta/ests$se
        fz <- sapply(support, function(x) {
          if (x < min(mus)) {
            return(0) # assume density is zero outside support for mus
          }
          else if (x > max(mus)) {
            return(0) # assume density is zero outside support for mus
          }
          return(mudens[which(abs(mus-x)==min(abs(mus-x)))])
        })
        inside <- fz/ests$se
        dens <- sum(inside)/length(ests$se)
        return(dens)
      }
      tmp <- sapply(betax, deltadens)
      betay <- betay + tmp * nrow(ests)/nrow(firm_effects)
      betay_list[[g]] <- tmp / sum(tmp) / width   
    }
    return(list(betay=betay,mudens_list=mudens_list,betay_list=betay_list,ll=ll))
  }

  # Tune Efron regularization parameter
  find_reg = function(c0) {
    betay <- get_g(c0)$betay
    implied_sd <- (sum(betax**2*betay)/sum(betay)-
                     (sum(betax*betay)/sum(betay))**2)
    gap = (implied_sd - second)**2*1000
    print(c0)
    print(gap)
    gap
  }
  o = optimize(find_reg, interval=c(0, .8))
  reg = o$minimum[[1]]

  # Efron density
  result <- get_g(reg)
  print(result$ll)
  betay <- result$betay
  mudens_list <- result$mudens_list
  betay_list <- result$betay_list

  # Plot quartile decomps
  betay_plots <- lapply(seq(1,ngroups),
                   function(g) {
                     ggplot() + geom_line(mapping = aes(x = betax, y = betay_list[[g]]),
                                          color = "blue", size=2) + 
                       geom_histogram(mapping=aes(x=split_ests[[g]]$est, y=..density..),
                                      color="red", fill="red", alpha=0.3, binwidth=0.01, center=0) +
                       labs(x = xlab, y = "Density") +
                       theme_minimal() + xlim(xlb, xub) +
                       theme(text = element_text(size=25))
                   })
  plot_grid(plotlist = betay_plots, ncol = 2)
  ggsave(paste("figures/figureE4_",tostudy,".pdf",sep=""),width=12, height=8,units = "in")

  # Recover density of deltas and implied moments
  betay <- betay / sum(betay) / width # make beta density integrate to 1
  implied_mean <- sum(betax*betay)/sum(betay)
  implied_var <- (sum(betax**2*betay)/sum(betay)-
                    (sum(betax*betay)/sum(betay))**2)
  implied_sd <- implied_var**0.5

  # Plot densities
  ggplot() + geom_line(mapping = aes(x = betax, y = betay),
                              color = "blue", size=2) + 
    geom_histogram(mapping=aes(x=firm_effects$est, y=..density..),
                   color="red", fill="red", alpha=0.3, binwidth=0.01, center=0) +
    labs(x = xlab, y = "Density") +
    annotate("text", label=paste("\n   Implied firm mean\n     gap:",
                                 format(sum(betax*betay)/sum(betay), digits=3),
                                 "\n   Implied between\n     firm SD:",
                                 format((sum(betax**2*betay)/sum(betay)-
                                           (sum(betax*betay)/sum(betay))**2)**0.5, digits=3)),
             size=8, x=-Inf, y=Inf,hjust=0,vjust=1) +
    theme_minimal() + xlim(xlb, xub) +
    theme(text = element_text(size=25))
  ggsave(paste("figures/figureE5_",tostudy,".pdf",sep=""),width=10, height=8,units = "in")

  # Construct lorenz curve
  if (tostudy == "white") {
    race_dens <- data.frame(betax,betay)
    race_dens$yield <- cumsum(abs(race_dens$betax)*race_dens$betay/sum(race_dens$betay)*1000)
    race_dens$yield <- race_dens$yield /max(race_dens$yield)
    race_dens$saved <- cumsum(race_dens$betay/sum(race_dens$betay))
    
    # 80/20 calc
    race_topq = min(race_dens[race_dens$saved >= 0.8,]$yield)
    
    lz <- ggplot() + geom_line(mapping = aes(x=saved, y=yield), data=race_dens, color = "blue", linetype="solid",
                               size=1.5, alpha=0.5) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1,), color="black", linetype="dashed") +
      geom_segment(aes(x = 0.8, y = race_topq, xend = 1, yend = race_topq), size=1, alpha=0.5, color="blue", linetype="dashed") +
      geom_text(aes(x=.95, y=race_topq, label=paste("Top 20%\nshare:",format(1-race_topq,digits=2))), size=5) +
      theme_minimal() +
      labs(x = "Share of firms", y = "Share of lost contacts") +
      theme(text = element_text(size=25), legend.key.width = unit(2, 'cm'), legend.position = c(0.75, 0.2))  

    # Gini coefficient
    under <- sum(race_dens$yield*race_dens$betay)/sum(race_dens$betay)
    gini_race <- 1-2*under
    
  } else {
    gend_dens <- data.frame(betax,betay)
    gend_dens$betax <- abs(gend_dens$betax)
    gend_dens <- gend_dens[order(gend_dens$betax),]
    gend_dens$yield <- cumsum(abs(gend_dens$betax)*gend_dens$betay/sum(race_dens$betay)*1000)
    gend_dens$yield <- gend_dens$yield /max(gend_dens$yield)
    gend_dens$saved <- cumsum(gend_dens$betay/sum(gend_dens$betay))
    
    gend_topq = min(gend_dens[gend_dens$saved >= 0.8,]$yield)
    
    # Gini coefficient
    under <- sum(gend_dens$yield*gend_dens$betay)/sum(gend_dens$betay)
    gini_gend <- 1-2*under
    
    # Graph
    lz + geom_line(mapping = aes(x=saved, y=yield), data=gend_dens, color = "red", linetype="twodash",
                   size=1.5, alpha=0.5) +
      geom_segment(aes(x = 0.8, y = gend_topq, xend = 1, yend = gend_topq), size=1, alpha=0.5, color="red", linetype="dashed") +
      geom_text(aes( x=.95, y=gend_topq, label=paste("Top 20%\nshare:",format(1-gend_topq,digits=2))), size=5) +
      annotate("segment", x = 0.78, xend = 0.75, y = 0.31, yend = 0.37, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=0.80, y=0.30, label=paste("Gender, Gini:",format(gini_gend,digits=3))), size=5) +
      annotate("segment", x = 0.51, xend = 0.54, y = 0.33, yend = 0.29, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=0.52, y=0.35, label=paste("Race, Gini:",format(gini_race,digits=3))), size=5) 
    ggsave(paste("figures/figureE6.pdf",sep=""))
  }
}

