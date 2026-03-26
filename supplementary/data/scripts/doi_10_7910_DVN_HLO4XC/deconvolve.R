# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code produce deconvolution estimates of population contact gaps
# and associated figures, as well as q-value calculation figures.

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
    gini_race_se <- 0.033874847301934
    race_topshare_se <- 0.0268514922610442
    } else {
    divider <- 1/35
    xlab <- "Firm male-female contact rate gap"
    ylab <- "Posterior mean male-female contact rate gap"
    leg_pos <- c(0.2, 0.9)
    xlb <- -0.2
    xub <- 0.15
    gend_topq_se <- 0.0186812056085065
    gini_gend_se <- 0.0192249678814415
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


  # Set up objects to feed into Efron procedure
  firm_effects$t_stat <- firm_effects$est / firm_effects$se
  zf <- firm_effects$est / firm_effects$se
  ses <- firm_effects$se

  # Efron density estimation parameters
  width <- .0001
  ntaus <- 5000

  if (tostudy == "white") {
    leftlim <-  0
    rightlim <- max(zf)+0.5
    betax <- seq(-0.05,max(zf)*max(firm_effects$se),by=width)
    blacklim <- c(-0.05,0.08)
  } else {
    leftlim <-  min(zf)-0.5
    rightlim <- max(zf)+0.5
    betax <- seq(min(zf)*max(firm_effects$se),max(zf)*max(firm_effects$se),by=width)
    blacklim <- c(-0.08,0.08)
  }
  tauwidth = (rightlim - leftlim)/ntaus
  mus <- seq(leftlim, rightlim, by=tauwidth)

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

  ### Figure 6: Deconvolution estimates of firm-level discrimination distributions
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

    mu = log(implied_mean)
    theta = log(implied_var)
    sdlog = log(exp(theta-2*mu)+1)
    meanlog = mu-sdlog/2
    plt + stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog**0.5), size=2, n=10000) + 
      annotate("segment", x = 0.065, xend = 0.048, y = 7, yend = 5, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=0.07, y=8, label="Log-normal density"), size=5)

  } else {
    plt <- plt + annotate("segment", x = -0.066, xend = -0.048, y = 8, yend = 6, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=-0.066, y=9, label="Observed gaps"), size=5) + 
      annotate("segment", x = -0.03, xend = -0.011, y = 20, yend = 18, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=-0.06, y=20.5, label="Deconvolved density"), size=5)    

    plt + stat_function(fun = dnorm, args = list(mean = 0, sd = implied_sd), size=2) +
      annotate("segment", x = 0.055, xend = 0.04, y = 7, yend = 5, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=0.065, y=8, label="Normal density"), size=5) +
      stat_function(fun = dLaplace, args = list(mean = 0, b = implied_sd/2**0.5), size=1.5,
                    color="steelblue", linetype="dashed", n=10000) +
      annotate("segment", x = 0.02, xend = 0, y = 28, yend = 26, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=0.035, y=29, label="Laplace density"), size=5) 
  }
    ggsave(paste("figures/figure6_",tostudy,".pdf",sep=""), 
           width = 10, height = 8, dpi = 120)


  ### Figure A10: Confidence intervals on deconvolutions of firm-level discrimination
  # Get standard errors on density
  fz_matrix <- function(delta) {
    fz <- sapply(firm_effects$se, function(se) {
      x <- delta/se
      if (x < min(mus)) {
        return(rep(0, length(mus))) 
      }
      else if (x > max(mus)) {
        return(rep(0, length(mus))) 
      }
      return((abs(mus-x)==min(abs(mus-x)))*1/se)
    })
    return(t(fz))
  }
  fz_full <- do.call(rbind, lapply(betax, fz_matrix)) 
  summer <- matrix(0,nrow(firm_effects)*length(betax),length(betax))
  for (k in 1:length(betax)) {
    r <- (k-1)*nrow(firm_effects) + 1
    r2 <- r + nrow(firm_effects) - 1
    summer[r:r2,k] <- 1/nrow(firm_effects)
  }
  summer <- Matrix(summer, sparse = TRUE)
  lhs <- t(summer) %*% fz_full
  betay_vcv <- lhs %*% result$cov.g %*% t(lhs) / betay_sum**2 / width**2
  betay_se <- diag(betay_vcv)**0.5

  # Plot density + standard errors
  ggplot() + geom_line(mapping = aes(x = betax, y = betay),
                       color = "blue", size=1, alpha=0.7) + 
    geom_line(mapping = aes(x = betax, y = betay-1.96*betay_se),
              color = "blue", linetype='dashed', size=1, alpha=0.7) + 
    geom_line(mapping = aes(x = betax, y = betay+1.96*betay_se),
              color = "blue", linetype='dashed', size=1, alpha=0.7) + 
    geom_histogram(mapping=aes(x=firm_effects$est,y=..density..),
                   color="red", fill="red", alpha=0.3, binwidth=0.01, center=0) +
    labs(x = xlab, y = "Density") +
    theme_minimal() + xlim(xlb, xub) +
    theme(text = element_text(size=25))
  ggsave(paste("figures/figureA10_",tostudy,".pdf",sep=""), 
         width = 10, height = 8, dpi = 120)


  ### Figure 7: Discrimination Lorenz Curves
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
      geom_text(aes(x=.95, y=1-race_topshare, label=paste("Top 20%\n",format(race_topshare,digits=2)," (",format(race_topshare_se,digits=2),")",sep="")), size=5) +
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
      geom_text(aes( x=.95, y=1-gend_topq, label=paste("Top 20%\n",format(gend_topq,digits=2)," (",format(gend_topq_se,digits=2),")", sep="")), size=5) +
      annotate("segment", x = 0.78, xend = 0.75, y = 0.31, yend = 0.37, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=0.80, y=0.27, label=paste("Gender, Gini:\n",format(gini_gend,digits=3)," (",format(gini_gend_se,digits=3),")", sep="")), size=5) +
      annotate("segment", x = 0.51, xend = 0.54, y = 0.33, yend = 0.29, size=1, alpha=0.8, arrow=arrow()) + 
      geom_text(aes( x=0.52, y=0.38, label=paste("Race, Gini:\n",format(gini_race,digits=3)," (",format(gini_race_se,digits=3),")", sep="")), size=5) 
    ggsave(paste("figures/figure7.pdf",sep=""),width=10, height=8,units = "in")
  }

  ### Figure 8: Posterior means by industry
  # Posterior for mu given each t-stat / z score
  postmean <- function(z) {
    postdens <- dnorm(z-mus)*mudens
    return(sum(postdens*mus)/sum(postdens))
  }

  firm_effects$post_mean_u <- sapply(zf, postmean)
  firm_effects$post_mean_beta <- firm_effects$post_mean_u*firm_effects$se

  # Posterior quantiles
  postquant <- function(z, alpha=0.1) {
    postdens <- dnorm(z-mus)*mudens
    postdens <- postdens/sum(postdens)
    postcdf <- cumsum(postdens)
    lower_ci <- mus[[which(abs(postcdf-alpha/2)==min(abs(postcdf-alpha/2)))]]
    upper_ci <- mus[[which(abs(postcdf-(1-alpha/2))==min(abs(postcdf-(1-alpha/2))))]]
    lower_ci2 <- mus[[which(abs(postcdf-alpha/4)==min(abs(postcdf-alpha/4)))]]
    upper_ci2 <- mus[[which(abs(postcdf-(1-alpha/4))==min(abs(postcdf-(1-alpha/4))))]]
    return(c("lower_ci"=lower_ci,"upper_ci"=upper_ci,"lower_ci2"=lower_ci2,"upper_ci2"=upper_ci2))
  }

  cis <- as.data.frame(do.call(rbind,lapply(firm_effects$t_stat,FUN=postquant)))
  firm_effects$lower_ci_u <- cis$lower_ci
  firm_effects$upper_ci_u <- cis$upper_ci
  firm_effects$lower_ci_beta <- firm_effects$lower_ci_u*firm_effects$se
  firm_effects$upper_ci_beta <- firm_effects$upper_ci_u*firm_effects$se
  firm_effects$lower_ci_beta95 <- cis$lower_ci2*firm_effects$se
  firm_effects$upper_ci_beta95 <- cis$upper_ci2*firm_effects$se

  # Linear shrinkage
  mean_beta = mean(firm_effects$est)
  btwn_firm = (1/nrow(firm_effects)*sum((firm_effects$est - mean_beta)**2) -
                 (nrow(firm_effects)-1)/nrow(firm_effects)**2*sum(firm_effects$se**2))
  wts <- btwn_firm/(firm_effects$se**2 + btwn_firm)
  firm_effects$normal_mean_beta <- firm_effects$est*wts + mean_beta*(1-wts)

  # SIC names
  tmp <- df %>% select(firm_id,sic_combined) %>% unique()
  firm_effects <- firm_effects %>% left_join(tmp, by="firm_id") 
  names <- read.csv("dump/sic_names.csv")
  firm_effects <- firm_effects %>% left_join(names, by="sic_combined") 

  # Save CSV
  write.csv(firm_effects,paste("dump/",tostudy,"_posterior_features.csv",sep=""))

  tmp <- firm_effects %>% group_by(sic_combined) %>%
    summarise(mean=mean(post_mean_beta),n02=sum(post_mean_beta >= 0.02),linear_mean=mean(normal_mean_beta))
  names <- read.csv("dump/sic_names.csv")
  tmp <- tmp %>% left_join(names, by="sic_combined") %>% unique()

  tmplong <- gather(tmp, est_types, est, c(mean,linear_mean))
  ggplot(data=tmplong) +
    geom_bar(aes(x=reorder(sic_name, est -1000*(est_types == 'mean'), max), y=est, fill=est_types),
             stat="identity", position="dodge", alpha=0.8) +
    coord_flip() +
    scale_fill_discrete(name = "", labels = c("Linear shrinkage","Posterior mean")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(x="", y=ylab, fill="") + theme_minimal() +  theme(text = element_text(size=25),
                                                           legend.position = leg_pos)
  ggsave(paste("figures/figure8_",tostudy,".pdf",sep=""), width = 12, height = 12)


  ### Figure A11: Distribution of observed, deconvolved, and posterior estimates
  ggplot() + stat_ecdf(mapping=aes(x=firm_effects$est, color="red", linetype="solid"),
                       geom = "step", size=1.5, alpha=0.5) +
    geom_line(mapping = aes(x = betax, y = cumsum(betay)/sum(betay), color = "blue", linetype="twodash"),
              size=1.5, alpha=0.5) +
    stat_ecdf(mapping=aes(x=firm_effects$post_mean_beta, color="green", linetype="dotted"),
              geom = "step", size=1.5, alpha=0.5) +
    stat_ecdf(mapping=aes(x=firm_effects$normal_mean_beta, color="deeppink", linetype="dashed"),
              geom = "step", size=1.5, alpha=0.5) +
    scale_colour_manual(name = '', breaks=c('red','blue','green','deeppink'),
                        values =c('red'='red','blue'='blue','green'='green','deeppink'='deeppink'),
                        labels = c('Observed gaps','Deconvolved prior','Posterior mean','Linear shrinkage')) +
    scale_linetype_manual(name = '', breaks=c("solid","twodash","dotted","dashed"),
                          values =c('solid'='solid','twodash'='twodash','dotted'='dotted','dashed'='dashed'),
                          labels = c('Observed gaps','Deconvolved prior','Posterior mean','Linear shrinkage')) +
    labs(x = xlab, y = "Cumulative probability") +
    geom_vline(xintercept=0, alpha=0.5) +
    guides(colour = guide_legend(override.aes = list(alpha = 0.2))) +
    theme_minimal() + coord_cartesian(xlim=blacklim) +
    theme(text = element_text(size=25), legend.key.width = unit(2, 'cm'), legend.position = c(0.75, 0.2))  
  ggsave(paste("figures/figureA11_",tostudy,".pdf",sep=""),
         width = 12, height = 8, dpi = 120)


  ### Figure 10: P-value distributions and local false discovery rates
  # Add qvalues
  if (tostudy == "white") {
    pvals <- c("ttest_p","ttest_p_greater")
  } else {
    pvals <- c("ttest_p")
  }

  for (touse in pvals) {
    qobj <- qvalue(firm_effects[[touse]], pi0.method="bootstrap")
    lambda <- qobj$lambda[[match(qobj$pi0, qobj$pi0.lambda)]]
    mini <- 0.12
    name <- "twosided"
    if (touse == "ttest_p_greater") {
      mini <- 0.18
      name <- "onesided"
    }

    plt <- ggplot(data=firm_effects) +
      geom_histogram(aes(x=.data[[touse]], y=..count../sum(..count..)/0.05),
                     color="#00BFC4", fill="#00BFC4", binwidth=0.05,
                     alpha=0.5, boundary=1, show.legend = FALSE) +
      labs( y = "Density",
            x = "P-value for test that firm discriminates") + theme_minimal() + theme(text = element_text(size=25))
    ymax <- max(ggplot_build(plt)$data[[1]]$y)
    plt <- plt + geom_segment(aes(x=0,xend=1,y=qobj$pi0,yend=qobj$pi0),
                              linetype="dashed", color='black',size=1) +
      geom_segment(aes(x=lambda,xend=lambda,y=0,yend=ymax),
                   linetype="dashed", color='steelblue',size=1) + 
      annotate("text", label=as.character(expression(hat(pi)[0]=="")), parse=T, size=9, x=1.07,y=qobj$pi0+.03) + 
      annotate("text", label=round(qobj$pi0,3), parse=T, size=7, x=1.06,y=qobj$pi0-mini) + 
      annotate("text", label=as.character(expression(lambda)), parse=T, size=8, x=lambda,y=-0.1)    
    plt + geom_line(aes(x=qobj$pvalues, y=qobj$lfdr), linetype="solid", color='red', size=2) +
      annotate("text", label="LFDR", parse=T, size=8, x=1.08,y=max(qobj$lfdr)) 
    ggsave(paste("figures/figure10_",tostudy,"_",name,".pdf",sep=""), height=11.0833, width=14, unit="in")
  }

  # Add to firm effects table
  firm_effects$qvalue = qobj$qvalues
  firm_effects$lfdr <- qobj$lfdr
  if (tostudy == "white") {
    race_firm_qvalues <- firm_effects %>% select(firm_id,qvalue,lfdr,post_mean_beta)
  } else {
    gender_firm_qvalues <- firm_effects %>% select(firm_id,qvalue,lfdr,post_mean_beta)
  }

  ### Table 9: Estimates of racial discrimination for firms with q-values below 0.05
  contract <- read.csv("covariates/federal_contractors.csv")
  firm_effects <- firm_effects %>% mutate(contractor = 1- firm_id %in% contract$firm_id)
  if (tostudy == "white") {
    tmp <- firm_effects %>% filter(qvalue <= 0.05) %>% arrange(qvalue, -est)
    write.csv(tmp,paste("dump/table9.csv",sep=""))
  }

  ### Figure A14: Posterior false discovery distribution among 23 firms with low q-values
  if (tostudy == "white") {
    # FDX control
    fdx <- function(gamma, alpha) {
      firm_effects$lfdr_rank <- rank(firm_effects$lfdr)
      firm_effects <- firm_effects %>% arrange(lfdr)
      
      fdx <- function(k) {
        ps <- firm_effects[1:k,"lfdr"]
        return(ppbinom(gamma*length(ps), ps, lower.tail=FALSE))
      }
      firm_effects$pdb <- sapply(seq(nrow(firm_effects)),fdx)
      return(sum(1-(cumsum(1-(firm_effects$pdb <= alpha)*1) > 0)*1))
    }

    # FDP CDF plot
    firm_effects$lfdr_rank <- rank(firm_effects$lfdr)
    firm_effects <- firm_effects %>% arrange(lfdr)
    ps <- firm_effects[1:23,"lfdr"]
    pmf <- sapply(seq(11)-1, function(x) dpbinom(floor(x), ps))
    cdf <- sapply(seq(11)-1, function(x) ppbinom(floor(x), ps))
    pmean <- sum(pmf*seq(0,10))
    ggplot() + geom_point(mapping = aes(x=seq(0,10), y = pmf, color="blue"),
                          size=2, alpha=0.8) + 
               geom_point(mapping = aes(x=seq(0,10), y = cdf, color="red"),
                   size=2, alpha=0.8, shape=0) +
      geom_vline(aes(xintercept=pmean), alpha=0.5, linetype="dashed") +
      guides(colour = guide_legend(override.aes = list(alpha = 0.8))) +
      scale_colour_manual(name = '', breaks=c('blue','red'),
                          values =c('blue'='blue','red'='red'),
                          labels = c("PMF","CDF"))  +
      geom_text(aes(x=pmean+0.7, y=0.6), label="Expected # of\n false discoveries", size=5) +
      annotate("segment", x =pmean+0.7, xend = pmean, y = 0.57, yend = 0.52, size=.8, alpha=0.8, arrow=arrow()) + 
      xlab("# of false discoveries") + ylab("Posterior mass / cumulative mass") +
      theme_minimal() +    theme(text = element_text(size=25), legend.key.width = unit(2, 'cm'), legend.position = c(.8,.8)) + 
        ylim(0,1) + 
      scale_x_continuous(breaks= pretty_breaks())
      ggsave(paste("figures/figureA14.pdf",sep=""),
      width = 16, height = 12)
  }

  ### Figure A15: Posterior mean contact gaps vs. q-values
  if (tostudy == "white") {
    ggplot(data=firm_effects) + geom_point(mapping = aes(x = qvalue, y = post_mean_beta),
                                           color = "blue", size=2) +
      geom_linerange(aes(x = qvalue, ymin = lower_ci_beta95, ymax = upper_ci_beta95),
                     color="steelblue",alpha=0.4) +
      labs(x = "q-value", y = ylab) + 
      theme_minimal() + theme(text = element_text(size=25))
      ggsave(paste("figures/figureA15.pdf",sep=""),
           width = 16, height = 12, dpi = 120)  
  }

  ### Figure 11: Detection tradeoffs
  if (tostudy == "white") {
    race_dens <- race_dens[order(-race_dens$betax),]
    race_dens$yield <- cumsum(abs(race_dens$betax)*race_dens$betay/sum(race_dens$betay)*1000)
    race_dens$saved <- 1-cumsum(race_dens$betay/sum(race_dens$betay))

    post_means <- data.frame("post_mean_beta"=sort(-firm_effects$post_mean_beta))
    post_means$yield <- cumsum(-post_means$post_mean_beta)/nrow(post_means)*1000
    post_means$yield <- post_means$yield * max(race_dens$yield)/max(post_means$yield)
    post_means$saved <- 1-seq.int(nrow(post_means))/nrow(post_means)

    lin_shrink <- firm_effects[order(-firm_effects$normal_mean_beta),]
    lin_shrink$yield <- cumsum(lin_shrink$post_mean_beta)/nrow(lin_shrink)*1000
    lin_shrink$yield <- lin_shrink$yield * max(race_dens$yield)/max(lin_shrink$yield)
    lin_shrink$saved <- 1-seq.int(nrow(lin_shrink))/nrow(lin_shrink)

    qval_rule <- firm_effects[order(firm_effects$qvalue),]
    qval_rule$yield <- cumsum(qval_rule$post_mean_beta)/nrow(qval_rule)*1000
    qval_rule$yield <- qval_rule$yield * max(race_dens$yield)/max(qval_rule$yield)
    qval_rule$saved <- 1-seq.int(nrow(qval_rule))/nrow(qval_rule)

    print(paste(tostudy,"share of conacts by top 23 posterior mean firms",max(head(post_means,23)$yield)/max(post_means$yield)))
    print(paste(tostudy,"share of conacts by firms with q-values <= 0.05",max(qval_rule[qval_rule$qvalue <= 0.05,'yield'])/max(qval_rule$yield)))

    labs = c('True contact gap (infeasible)','Posterior means','Linear shrinkage','q-value')
    plt <- ggplot() + geom_line(mapping = aes(x=saved, y=yield, color = "blue", linetype="solid"), data=race_dens,
                                size=1.5, alpha=0.6) +
      geom_line(mapping = aes(x=saved, y=yield, color = "red", linetype="twodash"), data=post_means,
                size=1.5, alpha=0.6) +
      geom_line(mapping = aes(x=saved, y=yield, color = "darkgreen", linetype="dotted"), data=lin_shrink,
                size=1.5, alpha=0.6) +
      geom_line(mapping = aes(x=saved, y=yield, color = "deeppink", linetype="dashed"), data=qval_rule,
                size=1.5, alpha=0.6) +
      geom_segment(aes(x = 0, y = max(race_dens$yield), xend = 1, yend = 0), color="black", linetype="dashed") +
      scale_colour_manual(name = '', breaks=c('blue','red','darkgreen','deeppink'),
                          values =c('blue'='blue','red'='red','darkgreen'='darkgreen','deeppink'='deeppink'),
                          labels = labs) +
      scale_linetype_manual(name = '', breaks=c("solid","twodash","dotted",'dashed'),
                            values =c('solid'='solid','twodash'='twodash',"dotted"="dotted",'dashed'='dashed'),
                            labels = labs) +
      theme_minimal() +
      guides(colour = guide_legend(override.aes = list(alpha = 0.3))) +
      labs(x = "Share of firms not investigated", y = "Callbacks saved per 1,000 apps") + ylim(0, 23) +
      theme(text = element_text(size=25), legend.key.width = unit(2, 'cm'), legend.position = c(0.25, 0.2))  
      ggsave(paste("figures/figure11.pdf",sep=""),
           width = 10, height = 8, dpi = 120)  
  }

}


### Table 10: Discrimination estimates and detection by industry
tmp <- df %>% distinct(firm_id,sic_combined)
race_sic_qv <- race_firm_qvalues %>% inner_join(tmp, by="firm_id") %>% mutate(
                  below05 = qvalue <= 0.05) %>% group_by(sic_combined) %>%
                summarize(nb05 = sum(below05), mldfr = mean(lfdr), mean_post = mean(post_mean_beta))
gender_sic_qv <- gender_firm_qvalues %>% inner_join(tmp, by="firm_id") %>% mutate(
            below05 = qvalue <= 0.05) %>% group_by(sic_combined) %>%
          summarize(nb05 = sum(below05), mldfr = mean(lfdr), mean_post = mean(post_mean_beta))

attributes <- c("white","male")
results = NULL
group_var <- "sic_combined"
  for (i in 1:length(attributes)) {
    char <- attributes[i]
    # LPM
    lpm <- lm.cluster(df, as.formula(
      paste("cb ~ factor(wave) + factor(",group_var,") + factor(",group_var,"):",
            char,"- 1",sep="")), "job_id")
    
    # ests
    firm_effects <- data.frame("lpm_est"=lpm$lm_res$coefficients) %>%
      rownames_to_column(var='coef') %>%  filter(grepl(paste(":",char,sep=""),coef)) %>%
      extract(coef, group_var, paste("\\(",group_var,"\\)([0-9]+)",sep=""), remove=TRUE)
    
    # ses
    firm_effects <- data.frame("lpm_se"=sqrt(diag(lpm$vcov))) %>%
      rownames_to_column(var='coef') %>%  filter(grepl(paste(":",char,sep=""),coef)) %>%
      extract(coef, group_var, paste("\\(",group_var,"\\)([0-9]+)",sep=""), remove=TRUE) %>%
      inner_join(firm_effects, by=group_var)
    tmp <- summary(lpm)
    
    firm_effects <- data.frame("lpm_p"=tmp[TRUE,"Pr(>|t|)"], coef=rownames(tmp)) %>%
      filter(grepl(paste(":",char,sep=""),coef)) %>%
      extract(coef, group_var, paste("\\(",group_var,"\\)([0-9]+)",sep=""), remove=TRUE) %>%
      inner_join(firm_effects, by=group_var)
    firm_effects[[group_var]] <- as.integer(firm_effects[[group_var]])

    firm_effects <- df %>% group_by(sic_combined) %>% summarize(
          nfirms=n_distinct(firm_id)) %>% inner_join(firm_effects, by=group_var)

    # Q values
    qobj <- qvalue(firm_effects$lpm_p, lambda = seq(0.05, max(firm_effects$lpm_p), 0.05))
    firm_effects$qval <- qobj$qvalues
    firm_effects$pi <- qobj$pi0
    firm_effects$attribute <- char

    # Add counts of firm q values
    if (char == "white") {
      firm_effects <- firm_effects %>% inner_join(race_sic_qv, by="sic_combined")
    } else {
      firm_effects <- firm_effects %>% inner_join(gender_sic_qv, by="sic_combined")
    }
    
    results <- rbind(results, firm_effects)
}
write_csv(results, "dump/table10.csv")

