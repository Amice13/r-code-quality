# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code produces NPMLE deconvolution estiamtes

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
library(ivreg)
library(gmm)
library(cowplot)
library(REBayes)

# Seed
set.seed(9905789) 

# Difference to study
charlist <- c("white", "male")
for (tostudy in charlist) {
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
  means <- df %>% group_by(firm_id, job_id, .data[[tostudy]]) %>% summarise(mean_cb=mean(cb))
  means <- means %>% spread(.data[[tostudy]],mean_cb) %>% drop_na()

  ttester <- function(fid) {
    tmp <- means %>% filter(firm_id == fid)
    test <- t.test(tmp[["1"]],tmp[["0"]], paired=TRUE)
    p <- test$p.value
    se <- test$stderr
    est <- test$estimate
    p_greater <- t.test(tmp[["1"]],tmp[["0"]], paired=TRUE, alternative="greater")$p.value
    p_less <- t.test(tmp[["1"]],tmp[["0"]], paired=TRUE, alternative="less")$p.value
    return(cbind(firm_id=fid, njobs=nrow(tmp), est=est, se=se, ttest_p=p, ttest_p_greater=p_greater, ttest_p_less=p_less))
  }

  firm_effects <- as.data.frame(do.call(rbind,lapply(unique(df$firm_id),FUN=ttester)))

  est <- firm_effects$est
  se <- firm_effects$se^2
  m <- firm_effects$njobs

  # Dependent mixture
  if (tostudy == "white") {
    u = seq(from = 0, to = max(est), length.out = 500)
  } else {
    u = 500
  }
  gldens <- GLVmix(est, se*m, m=m, u=u, v=500)
  supp_theta <- gldens$u
  supp_sigma <- gldens$v
  dens <- matrix(, nrow = length(supp_theta), ncol = length(supp_sigma))
  ns <- length(supp_theta)
  for (j in 1:length(supp_sigma)) {
      start <- (j-1)*ns+1
      end <- j*ns
      dens[,j] <- gldens$fuv[start:end]
  }

  betax = supp_theta
  betay = rowSums(dens)
  betay = betay/sum(betay)

  ### Figure E7: NPMLE deconvolution estimates of firm-level discrimination distributions
  plt <- ggplot() + geom_line(mapping = aes(x = betax, y = betay),
                              color = "blue", size=2) + 
    geom_histogram(mapping=aes(x=firm_effects$est,y=..count../sum(..count..)),
                   color="red", fill="red", alpha=0.3, binwidth=0.01, center=0) +
    labs(x = xlab, y = "Share of firms") +
    annotate("text", label=paste("\n   Implied firm mean\n     gap:",
                                 format(sum(betax*betay)/sum(betay), digits=3),
                                 "\n   Implied between\n     firm SD:",
                                 format((sum(betax**2*betay)/sum(betay)-
                                           (sum(betax*betay)/sum(betay))**2)**0.5, digits=3)),
             size=8, x=-Inf, y=Inf,hjust=0,vjust=1) +
    theme_minimal() + xlim(xlb, xub) +
    theme(text = element_text(size=25))

  if (tostudy == "white") {
    plt <- plt + annotate("segment", x = -0.04, xend = -0.025, y = .1, yend = .06, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=-0.04, y=.11, label="Observed gaps"), size=5) + 
      annotate("segment", x = 0.08, xend = 0.05, y = .13, yend = .1, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes(  x=0.08, y=.14, label="Deconvolved density"), size=5)     

  } else {
    plt <- plt + annotate("segment", x = -0.066, xend = -0.048, y = .08, yend = .06, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=-0.066, y=.09, label="Observed gaps"), size=5) + 
      annotate("segment", x = -0.03, xend = -0.011, y = .020, yend = .018, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=-0.06, y=.0205, label="Deconvolved density"), size=5)    

  }
    ggsave(paste("figures/figureE7",tostudy,".pdf",sep=""), 
           width = 10, height = 8, dpi = 120)


  ### Figure E8: NPMLE discrimination Lorenz Curves
  # Gini calculation
  calc_gini <- function(betay) {
    dens <- data.frame(betax,betay)
    dens$betax <- abs(dens$betax)
    dens <- dens[order(dens$betax),]
    yield  <- cumsum(abs(dens$betax)*dens$betay/sum(dens$betay))
    under <- sum(yield*dens$betay/max(yield))/sum(dens$betay)
    return(1-2*under)
  }

  # Lorenz curve
  if (tostudy == "white") {
    race_dens <- data.frame(betax,betay)
    race_dens$yield <- cumsum(abs(race_dens$betax)*race_dens$betay/sum(race_dens$betay)*1000)
    race_dens$yield <- race_dens$yield /max(race_dens$yield)
    race_dens$saved <- cumsum(race_dens$betay/sum(race_dens$betay))
    
    # Interpolated top share
    upper_lim <- min(race_dens$saved[race_dens$saved >= 0.8])
    lower_lim <- max(race_dens$saved[race_dens$saved < 0.8])
    upper_share <- min(race_dens$yield[race_dens$saved >= 0.8])
    lower_share <- max(race_dens$yield[race_dens$saved < 0.8])

    slope <- (upper_share - lower_share)/(upper_cdf - lower_cdf)
    race_topshare <- 1 - (slope*(.8-lower_lim) + lower_share)
    race_lim <- 0.8
    
    lz <- ggplot() + geom_line(mapping = aes(x=saved, y=yield), data=race_dens, color = "blue", linetype="solid",
                               size=1.5, alpha=0.5) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1,), color="black", linetype="dashed") +
      geom_segment(aes(x = race_lim, y = 1-race_topshare, xend = 1, yend = 1-race_topshare), size=1, alpha=0.5, color="blue", linetype="dashed") +
      geom_text(aes(x=.95, y=1-race_topshare+.05, label=paste("Top 20%\n",format(race_topshare,digits=2),sep="")), size=5) +
      theme_minimal() +
      labs(x = "Share of firms", y = "Share of lost contacts") +
      theme(text = element_text(size=25), legend.key.width = unit(2, 'cm'), legend.position = c(0.75, 0.2))  
    
    # Gini coefficient
    gini_race <- calc_gini(betay)
    
  } else {
    gend_dens <- data.frame(betax,betay)
    gend_dens$betax <- abs(gend_dens$betax)
    gend_dens <- gend_dens[order(gend_dens$betax),]
    gend_dens$yield <- cumsum(abs(gend_dens$betax)*gend_dens$betay/sum(race_dens$betay)*1000)
    gend_dens$yield <- gend_dens$yield /max(gend_dens$yield)
    gend_dens$saved <- cumsum(gend_dens$betay/sum(gend_dens$betay))
    
    # Interpolated top share
    upper_lim <- min(gend_dens$saved[gend_dens$saved >= 0.8])
    lower_lim <- max(gend_dens$saved[gend_dens$saved < 0.8])
    upper_share <- min(gend_dens$yield[gend_dens$saved >= 0.8])
    lower_share <- max(gend_dens$yield[gend_dens$saved < 0.8])

    slope <- (upper_share - lower_share)/(upper_cdf - lower_cdf)
    gend_topq <- 1 - (slope*(.8-lower_lim) + lower_share)
    gend_lim <- 0.8
    
    # Gini coefficient
    gini_gend <- calc_gini(betay)
    
    # Graph
    lz + geom_line(mapping = aes(x=saved, y=yield), data=gend_dens, color = "red", linetype="twodash",
                   size=1.5, alpha=0.5) +
      geom_segment(aes(x = gend_lim, y = 1-gend_topq, xend = 1, yend = 1-gend_topq), size=1, alpha=0.5, color="red", linetype="dashed") +
      geom_text(aes( x=.95, y=1-gend_topq-0.05, label=paste("Top 20%\n",format(gend_topq,digits=2), sep="")), size=5) +
      annotate("segment", x = 0.62, xend = 0.65, y = .15, yend = 0.23, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes(x=0.62, y=0.1, label=paste("Gender, Gini:\n",format(gini_gend,digits=3), sep="")), size=5) +
      annotate("segment", x = 0.71, xend = 0.75, y = 0.49, yend = 0.44, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes(x=0.7, y=0.53, label=paste("Race, Gini:\n",format(gini_race,digits=3), sep="")), size=5) +
    ggsave(paste("figures/figureE8.pdf",sep=""),width=10, height=8,units = "in")
  }
}
