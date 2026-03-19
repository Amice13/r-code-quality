##############################
#
# Replication file for helper functions in:
#
# Reining in the Rascals: Challenger Parties' Path to Power
#
# For publication in the the Journal of Politics
#
# Frederik Hjorth, Jacob Nyrup, & Martin Vinæs Larsen
# 
##################

#######################
### Set up functions ### ----
#######################

### "Not in" function

'%!in%' <- function(x,y)!('%in%'(x,y)) # Define function for not in

### RDD functions

# Set up rd_wrapper

rd_wrapper <-
  function(data){
    fit <- with(data, rdrobust::rdrobust(y = Y, x = X, kernel = "triangular", p = 1, bwselect = "mserd"))
    ret <- data.frame(rownames(fit$coef), fit$coef, fit$se, fit$z, fit$pv, fit$ci,fit$bws[1],fit$N_h[1],fit$N_h[2],fit$M[1],fit$M[2])
    row.names(ret) <- NULL
    names(ret) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high","bandwidth","nwithinbandwidth_control","nwithinbandwidth_treatment","n_control","n_treatment")
    ret
  }

# Set up function, which returns 1) plot, 2) rdd estimates, 3) bw-check, 4) balancecheck, 5) McCrary-test and 6) placebo test

rdd_full <- function(data,
                     title="",
                     xlab="Representation margin in election t",
                     ylab="Prob. of joining coalition at t+1",
                     xlim=c(-0.03,0.03),
                     ylim=c(-1,1)){
  
  data <- data %>% 
    dplyr::filter(mandates_calculated < 2) %>%
    dplyr::mutate(X = ifelse(mandates_calculated==1,share_threshold_loss,-share_threshold_gain)) %>% # X is the running variable
    dplyr::mutate(Z = as.factor(ifelse(X > 0,1,0))) %>% # Z is the cutoff
    dplyr::filter(!is.na(X)) %>% 
    dplyr::rename(Y = coalition)
  
  # Set up rd-regressions. These are defined in rd_wrapper
  rdoutput <- rd_wrapper(data)
  
  # Set up plot
  
  bws_out <- with(data, rdrobust::rdbwselect(y = Y, x = X))
  h = bws_out$bws[1]
  
  data <- data %>%
    
    dplyr::mutate(triangle_weights = if_else(abs(X) < h, 1- abs(X/h), 0))
  
  fit_rdd <- lm_robust(Y ~ Z + X + Z*X, weights = triangle_weights, data = data, subset = abs(X) < h)
  summary(fit_rdd)
  
  preds <- predict(fit_rdd, newdata = data, interval = "confidence") %>%
    as.data.frame() %>% 
    as_tibble() %>% 
    mutate(X=data$X,
           weights = data$triangle_weights)
  
  # Data frame with binned means
  binmeans <- data.frame(data) %>% 
    filter(X>min(xlim) & X<max(xlim)) %>% 
    mutate(bins=cut_interval(X,n=30)) %>% 
    group_by(bins) %>% 
    summarise(binxmean=mean(X,na.rm=TRUE),
              binymean=mean(Y,na.rm=TRUE),
              meanwt=mean(triangle_weights)) %>% 
    ungroup()
  
  plot_rdd <- ggplot(data=preds, aes(x = X, y = fit.fit)) +
    geom_point(data=binmeans,aes(x=binxmean,y=binymean,size=meanwt,alpha = 0.05)) +
    geom_line(data=subset(preds,X<0)) + 
    geom_line(data=subset(preds,X>0)) + 
    geom_ribbon(aes(ymax = fit.upr, ymin = fit.lwr), fill = "grey", alpha = 0.2) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    theme_bw() + 
    theme(legend.position="none") + 
    labs(title = title) + 
    xlim(xlim) + 
    ylim(ylim) +
    ylab(ylab) + 
    xlab(xlab) +
    coord_cartesian(ylim=c(-.2,0.6)) # Crop to narrower y limits, keeping CI bands
  
  # Save elements
  
  outlist <- list(rdoutput,plot_rdd)
  
  return(outlist)
  
}

### Function for balance tests ---

rdd_full_balance <- function(data,
                             depvar,
                             electiont=FALSE,
                             title="",
                             xlab="Representation margin in election t",
                             ylab="Dependent variable",
                             xlim=c(-0.03,0.03),
                             ylim=c(-1,1),
                             mandates_this = 0){
  
  data1 <- data %>% 
    dplyr::filter(mandates_calculated < 2) %>%
    dplyr::mutate(X = ifelse(mandates_calculated==1,share_threshold_loss,-share_threshold_gain)) %>% # X is the running variable
    dplyr::mutate(Z = as.factor(ifelse(X > 0,1,0))) %>%
    dplyr::filter(!is.na(X)) 
  
  data1$Y <- unlist(data1[depvar])
  
  if(electiont){
    data <- data1 %>% 
      dplyr::mutate(X = ifelse(mandates_actual==1,share_threshold_loss,-share_threshold_gain)) # X is the running variable
  } else {
    data <- data1
  }
  
  # Set up rd-regressions. These are defined in rd_wrapper
  rdoutput <- rd_wrapper(data)
  
  # Set up plot
  
  bws_out <- with(data, rdrobust::rdbwselect(y = Y, x = X))
  h = bws_out$bws[1]
  
  data <- data %>%
    dplyr::mutate(triangle_weights = if_else(abs(X) < h, 1- abs(X/h), 0))
  
  fit_rdd <- lm_robust(Y ~ Z + X, weights = triangle_weights, data = data, subset = abs(X) < h)
  summary(fit_rdd)
  
  preds <- predict(fit_rdd, newdata = data, interval = "confidence") %>%
    as.data.frame() %>% 
    as_tibble() %>% 
    mutate(X=data$X,
           weights = data$triangle_weights)
  
  #data frame with binned means
  binmeans <- data.frame(data) %>% 
    filter(X>min(xlim) & X<max(xlim)) %>% 
    mutate(bins=cut_interval(X,n=30)) %>% 
    group_by(bins) %>% 
    summarise(binxmean=mean(X,na.rm=TRUE),
              binymean=mean(Y,na.rm=TRUE),
              meanwt=mean(triangle_weights)) %>% 
    ungroup()
  
  plot_rdd <- ggplot(data=preds, aes(x = X, y = fit.fit)) +
    geom_point(data=binmeans,aes(x=binxmean,y=binymean,size=meanwt),alpha = 0.05) +
    geom_line(data=subset(preds,X<0)) + 
    geom_line(data=subset(preds,X>0)) + 
    geom_ribbon(aes(ymax = fit.upr, ymin = fit.lwr), fill = "grey", alpha = 0.2) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    theme_bw() + 
    theme(legend.position="none") + 
    labs(title = title) + 
    xlim(xlim) + 
    ylim(ylim) +
    ylab(ylab) + 
    xlab(xlab) 

  # Save elements
  
  outlist <- list(rdoutput,plot_rdd)
  
  return(outlist)
  
}

### Function for analysis where t-1 is the threshold ---

rdd_full_retro <- function(data,
                     title="",
                     xlab="Representation margin in election t-1",
                     ylab="Prob. of joining coalition after election t",
                     xlim=c(-0.03,0.03),
                     ylim=c(-1,1)){
  
  data <- data %>% 
    dplyr::filter(lagmandates < 2 & mandates_calculated > 0) %>%
    dplyr::mutate(X = ifelse(lagmandates==1,lag_sharethreshold_loss,-lag_sharethreshold_gain)) %>% # X is the running variable
    dplyr::mutate(Z = as.factor(ifelse(X > 0,1,0))) %>% # Z is the cutoff
    dplyr::filter(!is.na(X)) %>% 
    dplyr::rename(Y = coalition)
  
  # Set up rd-regressions. These are defined in rd_wrapper
  rdoutput <- rd_wrapper(data)
  
  # Set up plot
  
  bws_out <- with(data, rdrobust::rdbwselect(y = Y, x = X))
  h = bws_out$bws[1]
  
  data <- data %>%
    dplyr::mutate(triangle_weights = if_else(abs(X) < h, 1- abs(X/h), 0))
  
  fit_rdd <- lm_robust(Y ~ Z + Z*X, weights = triangle_weights, data = data, subset = abs(X) < h)
  summary(fit_rdd)
  
  preds <- predict(fit_rdd, newdata = data, interval = "confidence") %>%
    as.data.frame() %>% 
    as_tibble() %>% 
    mutate(X=data$X,
           weights = data$triangle_weights)
  
  #data frame with binned means
  binmeans <- data %>% 
    filter(X>min(xlim) & X<max(xlim)) %>% 
    mutate(bins=cut(X,20)) %>% 
    group_by(bins) %>% 
    summarise(binxmean=mean(X),
              binymean=mean(Y),
              meanwt=mean(triangle_weights))
  
  plot_rdd <- ggplot(data=preds, aes(x = X, y = fit.fit)) +
    geom_point(data=binmeans,aes(x=binxmean,y=binymean,size=meanwt,alpha = 0.05)) +
    geom_line(data=subset(preds,X<0)) + 
    geom_line(data=subset(preds,X>0)) + 
    geom_ribbon(aes(ymax = fit.upr, ymin = fit.lwr), fill = "grey", alpha = 0.2) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    theme_bw() + 
    theme(legend.position="none") + 
    labs(title = title) + 
    xlim(xlim) + 
    ylim(ylim) +
    ylab(ylab) + 
    xlab(xlab) +
    coord_cartesian(ylim=c(-.2,.6)) # Crop to narrower y limits, keeping CI bands
  
  # 
  
  # Save elements
  
  outlist <- list(rdoutput,plot_rdd)
  
  return(outlist)
  
}

### Canvas for placebo-tests ---

canvas_placebo <- function(df=df,x = cutoff, y=estimate,se=se){
  ggplot(df, aes(x=cutoff)) + 
    xlab("Used cutoff") +
    ylab("Size of estimate") +
    ggtitle("") +
    expand_limits(y=-1:1) +                        # Expand y range
    scale_y_continuous(lim=c(-1,1)) +
    scale_x_reverse() +
    theme_bw(base_size = 9) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "black", size=1)  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "") +
    geom_vline(xintercept = 0, linetype="dotted", 
               color = "black", size=1) +
    geom_path(aes(y=estimate,group=1,color = if_else(estimate-se*1.96>0 | estimate+se*1.96<0,"color1","color2"))) +
    geom_path(aes(y=estimate+se*1.96,group=1,color = if_else(estimate-se*1.96>0 | estimate+se*1.96<0,"color1","color2"))) +
    geom_path(aes(y=estimate-se*1.96,group=1,color = if_else(estimate-se*1.96>0 | estimate+se*1.96<0,"color1","color2"))) +
    scale_colour_manual(values=c("red","black")) 
}

### Canvas for bandwidth tests ---

canvas_bandwidths <- function(df=df,x = bandwidth, y=estimate,se=se, bwused = bwused){
  ggplot(df, aes(x=bandwidth)) + 
    xlab("Used bandwidth") +
    ylab("Size of estimate") +
    ggtitle("") +
    xlim(0.000,0.03) +
    scale_y_continuous(lim=c(-0.8,0.8)) +
    theme_bw(base_size = 9) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "black", size=1)  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "") +
    geom_vline(xintercept = bwused, linetype="dotted", 
               color = "black", size=1) +
    annotate("text",x = bwused+0.0035, y = 0.5, label = "MSE optimal bandwidth") +
    geom_path(aes(y=estimate,group=1,group=1)) +
    geom_path(aes(y=estimate+se*1.96, type = "dashed",group=1)) +
    geom_path(aes(y=estimate-se*1.96, type = "dashed",group=1)) +
    scale_colour_manual(values=c("red","black"))
}

### Set up rd_wrapper for alternative specifications ---

rd_wrapper_alt <- 
  function(data, poly = 1, kernel_spec = "triangular"){
    fit <- with(data, rdrobust::rdrobust(y = Y, x = X, kernel = kernel_spec, p = poly, bwselect = "mserd"))
    ret <- data.frame(rownames(fit$coef), fit$coef, fit$se, fit$z, fit$pv, fit$ci,fit$bws[1],fit$N_h[1],fit$N_h[2],fit$M[1],fit$M[2])
    row.names(ret) <- NULL
    names(ret) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high","bandwidth","nwithinbandwidth_control","nwithinbandwidth_treatment","n_control","n_treatment")
    ret
  }

### Set up rd_wrapper for controls ---

rd_wrapper_control <- 
  function(data, control = NULL){
    fit <- with(data, rdrobust::rdrobust(y = Y, x = X, kernel = "triangular", p = 1, bwselect = "mserd", covs = control))
    ret <- data.frame(rownames(fit$coef), fit$coef, fit$se, fit$z, fit$pv, fit$ci,fit$bws[1],fit$N_h[1],fit$N_h[2],fit$M[1],fit$M[2])
    row.names(ret) <- NULL
    names(ret) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high","bandwidth","nwithinbandwidth_control","nwithinbandwidth_treatment","n_control","n_treatment")
    ret
  }
