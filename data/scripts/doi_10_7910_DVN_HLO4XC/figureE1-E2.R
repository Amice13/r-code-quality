# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code produces variance-stabilized deconvolution estimates.

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

# Seed
set.seed(205789) 

# Difference to study
charlist <- c("white", "male")
for (tostudy in charlist) {
  pd <- 5
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
    return(cbind(firm_id=fid, est=est, se=se, ttest_p=p, ttest_p_greater=p_greater, ttest_p_less=p_less))
  }

  firm_effects <- as.data.frame(do.call(rbind,lapply(unique(df$firm_id),FUN=ttester)))
  first = mean(firm_effects$est)
  second = (1/(nrow(firm_effects))*sum((firm_effects$est - first)**2) -
              (nrow(firm_effects)-1)/(nrow(firm_effects)**2)*sum(firm_effects$se**2))

  # Linear model
  transf_deriv <- function(x) {(a + b*x + c*x**2)**(-0.5)}
  transf <- function(x){integrate(transf_deriv, -10, x, subdivisions=1000)$value}

  # SSIV params
  nloops <- 1000
  a <- c()
  b <- c()
  c <- c()
  for (val in 1:nloops) {
    print(paste("Working on loop ",val))
    
    # Split sample
    jids <- df %>% select(firm_id, job_id) %>% unique()
    split1 <- jids %>% group_by(firm_id) %>% sample_frac(0.5)
    split2 <-  jids %>% filter(!job_id %in% split1$job_id)
    split1 <- split1 %>% left_join(df, by=c("firm_id","job_id"))
    split2 <- split2 %>% left_join(df, by=c("firm_id","job_id"))
    
    means <- split1 %>% group_by(firm_id, job_id, .data[[tostudy]]) %>% summarize(mean_cb=mean(cb))
    means <- means %>% spread(.data[[tostudy]],mean_cb) %>% drop_na()
    firm_effects1 <- as.data.frame(do.call(rbind,lapply(unique(split1$firm_id),FUN=ttester)))
    
    means <- split2 %>% group_by(firm_id, job_id, .data[[tostudy]]) %>% summarize(mean_cb=mean(cb))
    means <- means %>% spread(.data[[tostudy]],mean_cb) %>% drop_na()
    firm_effects2 <- as.data.frame(do.call(rbind,lapply(unique(split2$firm_id),FUN=ttester)))
    
    # Join to run SSIV
    split1 <- firm_effects1 %>% select(firm_id, se, est) %>%
      left_join(firm_effects2[c('firm_id','est')], by='firm_id')
    split2 <- firm_effects2 %>% select(firm_id, se, est) %>%
      left_join(firm_effects1[c('firm_id','est')], by='firm_id')
    
    split <- rbind(split1, split2)
    split <- split %>% left_join(firm_effects[c('firm_id','se')], by='firm_id')
    split$var = split$se.y**2
    split$est2 = split$est.x**2 - split$se.x**2
    split$inst = split$est.y**2
    firm_effects$t_stat = firm_effects$est / firm_effects$se
    firm_effects$est2 <- firm_effects$est**2 - firm_effects$se**2
    firm_effects$est3 <- firm_effects$est**3 - 2*firm_effects$se**2*firm_effects$est

    # Log model
    mod <- ivreg(var ~ est.x + est2 | est.y + inst, data=split)
    print(mod)
    a[val] <- coef(mod)['(Intercept)']
    b[val] <- coef(mod)['est.x']
    c[val] <- coef(mod)['est2']
    
  }
  a <- median(a)
  b <- median(b)
  c <- median(c)
  print(c(a,b,c))

  # Set up objects to feed into Efron procedure
  firm_effects$t_stat <- firm_effects$est / firm_effects$se
  zf <- sapply(firm_effects$est, transf)

  # Efron density estimation parameters
  width <- .0001
  ntaus <- 5000

  if (tostudy == "white") {
    leftlim <-  transf(0)
    rightlim <- max(zf)+1
    betax <- seq(-0.05,max(firm_effects$t_stat)*max(firm_effects$se),by=width)
    blacklim <- c(-0.05,0.08)
  } else {
    leftlim <-  min(zf)-1
    rightlim <- max(zf)+1
    betax <- seq(min(firm_effects$t_stat)*max(firm_effects$se),max(firm_effects$t_stat)*max(firm_effects$se),by=width)
    blacklim <- c(-0.08,0.08)
  }
  tauwidth = (rightlim - leftlim)/ntaus
  tau <- seq(leftlim, rightlim, by=tauwidth)

  # Get density of betas directly
  deltadens <- function(delta, mudens){
    support <- transf(delta)
    change <- function(x) {
      if (x < min(tau)) {
        return(0) # assume density is zero outside support for mus
      }
      else if (x > max(tau)) {
        return(0) # assume density is zero outside support for mus
      }
      return(mudens[which(abs(tau-x)==min(abs(tau-x)))])
    }
    dens <- change(support)*transf_deriv(delta)
    return(dens)
  }

  # Return G
  get_g <- function(c0) {
    res <- deconv(tau=tau, X=zf, family="Normal",
                  c0=c0, pDegree=pd) 
    mudens <- res$stats[, "g"]
    betay <- sapply(betax, deltadens, mudens=mudens)
    return(list(res=res,betay=betay))
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

  ### Figure E1-D2: VST deconvolution estimates of firm-level discrimination distributions
  plt <- ggplot() + geom_line(mapping = aes(x = betax, y = betay),
                              color = "blue", size=2) + 
    geom_histogram(mapping=aes(x=firm_effects$est,y=..density..),
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

  if (tostudy == "white") {
    plt <- plt + annotate("segment", x = -0.04, xend = -0.025, y = 8, yend = 6, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=-0.04, y=9, label="Observed gaps"), size=5) + 
      annotate("segment", x = 0.045, xend = 0.028, y = 23, yend = 20, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes(  x=0.065, y=24, label="Deconvolved density"), size=5)     

  } else {
    plt <- plt + annotate("segment", x = -0.066, xend = -0.048, y = 8, yend = 6, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=-0.066, y=9, label="Observed gaps"), size=5) + 
      annotate("segment", x = -0.03, xend = -0.011, y = 20, yend = 18, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=-0.06, y=20.5, label="Deconvolved density"), size=5)    

  }
    ggsave(paste("figures/figureE1_",tostudy,".pdf",sep=""), 
           width = 10, height = 8, dpi = 120)


  ### Figure E2: VST discrimination Lorenz Curves
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

  # Lorenz curve
  if (tostudy == "white") {
    race_dens <- data.frame(betax,betay)
    race_dens$yield <- cumsum(abs(race_dens$betax)*race_dens$betay/sum(race_dens$betay)*1000)
    race_dens$yield <- race_dens$yield /max(race_dens$yield)
    race_dens$saved <- cumsum(race_dens$betay/sum(race_dens$betay))
    
    race_topshare <- calc_topshare(betay)
    
    lz <- ggplot() + geom_line(mapping = aes(x=saved, y=yield), data=race_dens, color = "blue", linetype="solid",
                               size=1.5, alpha=0.5) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1,), color="black", linetype="dashed") +
      geom_segment(aes(x = 0.8, y = 1-race_topshare, xend = 1, yend = 1-race_topshare), size=1, alpha=0.5, color="blue", linetype="dashed") +
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
    
    # Delta method SE on top 20% share
    gend_topq <- calc_topshare(betay)
    
    # Gini coefficient
    gini_gend <- calc_gini(betay)
    
    # Graph
    lz + geom_line(mapping = aes(x=saved, y=yield), data=gend_dens, color = "red", linetype="twodash",
                   size=1.5, alpha=0.5) +
      geom_segment(aes(x = 0.8, y = 1-gend_topq, xend = 1, yend = 1-gend_topq), size=1, alpha=0.5, color="red", linetype="dashed") +
      geom_text(aes( x=.95, y=1-gend_topq-0.05, label=paste("Top 20%\n",format(gend_topq,digits=2), sep="")), size=5) +
      annotate("segment", x = 0.78, xend = 0.75, y = 0.31, yend = 0.37, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=0.80, y=0.3, label=paste("Gender, Gini:\n",format(gini_gend,digits=3), sep="")), size=5) +
      annotate("segment", x = 0.62, xend = 0.56, y = .12, yend = 0.18, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=0.62, y=0.1, label=paste("Race, Gini:\n",format(gini_race,digits=3), sep="")), size=5) 
    ggsave(paste("figures/figureE2.pdf",sep=""),width=10, height=8,units = "in")
  }
}
