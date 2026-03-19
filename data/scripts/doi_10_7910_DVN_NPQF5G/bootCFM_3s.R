# Bootstrapped CF Model


bootCFM <- function(prob_sampled_by_strata = .8, n_reps = 1000, form, dat, strata_var, method = c("efron"), x = TRUE,  y = TRUE,
                    export_ParametricCI_plot = TRUE, 
                    export_NonParametricCI_plot = TRUE,
                    export_EmpDist_plot = FALSE, plot_title_EmpDist = "plotBootCFM_EmpDist.pdf", seed = 12345, ...)
{
  require(survival)
  library(ggplot2)
  library(splines)
  library(ggplot2)
  library(RColorBrewer)
  set.seed(seed)
  out <- list()
  coef_mat <- data.frame()
  ch1 <- list()
  ch2 <- list()
  ch3 <- list()
  # ch4 <- list()
  
  # first, start the loop for each bootstrap replication
  for(i in 1:n_reps){
    # split up the strata to create new_dataset
    rep_dataset <- data.frame()
    for(s in 1:length(unique(strata_var))){
      strata_df <- dat[strata_var == unique(strata_var)[s],]
      flips <- rbinom(n = nrow(strata_df), size = 1, prob = prob_sampled_by_strata)
      strata_df <- strata_df[which(flips == 1), ]
      rep_dataset <- rbind(rep_dataset, strata_df)
    }
    # fit model
    cfm.fit <- coxph(form,
                     method = method,
                     x = x, 
                     y = y,
                     data = rep_dataset)
    
    # save coefficients
    coefs <- coef(cfm.fit)
    
    coef_mat <- rbind(coef_mat, coefs)
    
    colnames(coef_mat) <- names(coefs)
    
    # save cumulative hazards
    plot_df <- data.frame(cumhaz = survfit(cfm.fit)$cumhaz,
                          strata8 = c(rep(1, survfit(cfm.fit)$strata[1]),
                                      rep(2, survfit(cfm.fit)$strata[2]),
                                      rep(3, survfit(cfm.fit)$strata[3])),
                          time = survfit(cfm.fit)$time)
    
    ch1[[i]] <- data.frame(time = plot_df$time[which(plot_df$strata8 == 1)],
                           cumhas = plot_df$cumhaz[which(plot_df$strata8 == 1)],
                           col = 1)
    ch2[[i]] <- data.frame(time = plot_df$time[which(plot_df$strata8 == 2)],
                           cumhas = plot_df$cumhaz[which(plot_df$strata8 == 2)],
                           col = 2)
    ch3[[i]] <- data.frame(time = plot_df$time[which(plot_df$strata8 == 3)],
                           cumhas = plot_df$cumhaz[which(plot_df$strata8 == 3)],
                           col = 3)
    
  }
  # compile output list
  out$coef_dist <- coef_mat
  out$n_reps <- n_reps
  out$sampling_prob <- prob_sampled_by_strata
  out$formula <- form
  out$cum_haz_1 <- ch1
  out$cum_haz_2 <- ch2
  out$cum_haz_3 <- ch3
  
  chlist <- list(ch1, ch2, ch3)
  # Now, I want to find all of the distinct time periods estimated
  times_list <- list()
  for(i in 1:length(chlist)){
    cum_hz_strat <- chlist[[i]]
    times <- NULL
    for(j in 1:n_reps){
      rep_times_str <- cum_hz_strat[[j]]$time
      times <- append(times, rep_times_str)
    }
    times <- sort(unique(times))
    
    # create a times matrix that is T x n_reps
    matrix <- matrix(nrow = length(times), ncol = n_reps)
    rownames(matrix) <- times
    
    times_list[[i]] <- matrix
  }
  
  # Now, for each of the strata I need to loop through all replications and find the parametric distributions by time-period
  ch_dist_list <- list()
  for(i in 1:length(times_list)){
    strata_list <- chlist[[i]]
    time_matrix <- times_list[[i]]
    for(j in 1:n_reps){
      rep_df <- strata_list[[j]]
      row_indicies <- which(strata_list[[j]]$time %in% rownames(time_matrix))
      time_matrix[row_indicies, j] <- rep_df$cumhas
    }
    ch_dist_list[[i]] <- time_matrix
  }
  
  plotting_dataframe <- data.frame()
  # Now, I need to collapse these down to get means and parametric 95% confidnece intervals
  for(i in 1:length(ch_dist_list)){
    strat_dist <- ch_dist_list[[i]]
    means <- apply(strat_dist, 1, function(x) mean(x, na.rm = TRUE))
    errors <- apply(strat_dist, 1, function(x) sd(x, na.rm = TRUE))
    strata_plot_df <- data.frame(
      time = rownames(strat_dist),
      mean = means,
      low95 = means-(1.96*errors),
      high95 = means+(1.96*errors),
      strata = i
    )
    plotting_dataframe <- rbind(plotting_dataframe, strata_plot_df)
  }
  
  out$cumhazParametricCI <- plotting_dataframe
  
  if(export_ParametricCI_plot == TRUE){
    pal <- brewer.pal(s, "Set2")
    pal_rev <- rev(pal)
    
    p <- ggplot(data = plotting_dataframe, aes(as.numeric(time), mean)) +
      geom_line(aes(group = strata, col = factor(strata)), size = 1.5) +
      geom_vline(xintercept = 18, colour = "darkgrey", size = 1, linetype = 2) +
      geom_ribbon(aes(group = strata, ymin=low95, ymax=high95, fill = factor(strata)), alpha = 0.25)  + 
      scale_color_manual(name = "Strata", 
                         values = pal_rev) +
      scale_fill_manual(values = pal_rev, name="Strata") + 
      theme_bw() +
      ylab('Cumulative Hazard') +
      xlab("Time (Months)") +
      scale_y_continuous(breaks=seq(0,0.525,0.1), limits = c(0, 0.525)) +
      scale_x_continuous(breaks =c(6, 12, 18, 24, 30), limits = c(6, 32)) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
    
    p

    out$CI_plot <- p
  }
  
  
  quantile_dataframe <- data.frame()
  # Get non-parametric 95 percent confidence intervals
  for(i in 1:length(ch_dist_list)){
    strat_dist <- ch_dist_list[[i]]
    median <- apply(strat_dist, 1, function(x) quantile(x, c(0.5), na.rm = TRUE))
    low95 <- apply(strat_dist, 1, function(x) quantile(x, c(0.025), na.rm = TRUE))
    high95 <- apply(strat_dist, 1, function(x) quantile(x, c(0.975), na.rm = TRUE))
    strata_plot_df <- data.frame(
      time = rownames(strat_dist),
      mean = median,
      low95 = low95,
      high95 = high95,
      strata = i
    )
    quantile_dataframe <- rbind(quantile_dataframe, strata_plot_df)
  }
  
  out$cumhazNonParametricCI <- quantile_dataframe
  
  
  if(export_NonParametricCI_plot == TRUE){
    pal <- brewer.pal(s, "Set2")
    pal_rev <- rev(pal)
    
    p <- ggplot(data = quantile_dataframe, aes(as.numeric(time), mean)) +
      geom_line(aes(group = strata, col = factor(strata)), size = 1.5) +
      geom_vline(xintercept = 18, colour = "darkgrey", size = 1, linetype = 2) +
      geom_ribbon(aes(group = strata, ymin=low95, ymax=high95, fill = factor(strata)), alpha = 0.25)  + 
      scale_color_manual(name = "Strata", 
                         values = pal_rev) +
      scale_fill_manual(values = pal_rev, name="Strata") + 
      theme_bw() +
      ylab('Cumulative Hazard') +
      xlab("Time (Months)") +
      scale_y_continuous(breaks=seq(0,0.525,0.1), limits = c(0, 0.525)) +
      scale_x_continuous(breaks =c(6, 12, 18, 24, 30), limits = c(6, 32)) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
    

    
    out$NPCI_plot <- p
  }
  
  #go ahead and write the plot if true
  if(export_EmpDist_plot == TRUE){
    pal <- brewer.pal(s, "Set2")
    pal_rev <- rev(pal)
    p <- ggplot() 
    for(i in 1:n_reps){
      p <- p +
        layer(data = ch1[[i]], geom = "line", mapping = aes(x = time, y = cumhas, colour= "Strata 1"), params = list(alpha = 0.25, size = 0.25), stat = "identity", position = "identity") +
        layer(data = ch2[[i]], geom = "line", mapping = aes(x = time, y = cumhas, colour= "Strata 2"), params = list(alpha = 0.25, size = 0.25), stat = "identity", position = "identity") +
        layer(data = ch3[[i]], geom = "line", mapping = aes(x = time, y = cumhas, colour= "Strata 3"), params = list(alpha = 0.25, size = 0.25), stat = "identity", position = "identity") 
  #      layer(data = ch4[[i]], geom = "line", mapping = aes(x = time, y = cumhas, colour= "Strata 4"), params = list(alpha = 0.25, size = 0.25), stat = "identity", position = "identity")  
      
    }
    p <- p +
      scale_colour_manual(name = 'Strata', 
                          guide = "legend",
                          values = c(pal_rev[1], pal_rev[2], pal_rev[3])) +
      ylab('Cumulative Hazard') +
      xlab('Time (Month)') +
      theme_bw() + 
      theme(legend.position = c(0.1, 0.8), 
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
    
    pdf(plot_title_EmpDist)
    p
    dev.off()
  }
  return(out)
}


