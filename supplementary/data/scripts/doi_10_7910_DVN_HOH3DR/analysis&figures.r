rm(list=ls(all=TRUE))

library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(ggplot2)
library(matrixStats)
library(ggpubr)
library(zoo)
library(forcats)
library(mgcv)
library(stargazer)
library(ggrepel) 


################## ################## ################## ################## 
#### DEFINE FUNCTIONS USED IN ANALYSIS
################## ################## ################## ################## 


## Define the loess.boot function which fits a loess curve and then calculates a
## symmetric nonparametric bootstrap with a confidence region.
## Function is adapted from the spatialEco package (https://rdrr.io/github/jeffreyevans/spatialEco/man/loess.boot.html),
loess.boot <- function (x, y, nreps = 100, confidence = 0.95, ...) 
{
  dat <- stats::na.omit(data.frame(x = x, y = y))
  if (nrow(dat) == 0) 
    stop("Error in dropping NA's")
  ndx <- order(dat$x)
  dat$x <- dat$x[ndx]
  dat$y <- dat$y[ndx]
  r <- range(dat$x, na.rm = TRUE)
  x.out <- seq(r[1], r[2], length.out = 500)
  f <- stats::loess(y ~ x, data = dat, ...)
  y.fit <- stats::approx(f$x, stats::fitted(f), x.out, rule = 2)$y
  len <- length(dat$x)
  mat <- matrix(0, nreps, length(x.out))
  for (i in seq(nreps)) {
    ndx <- sample(len, replace = TRUE)
    x.repl <- x[ndx]
    y.repl <- y[ndx]
    f <- stats::loess(y.repl ~ x.repl, ...)
    mat[i, ] <- stats::predict(f, newdata = x.out)
  }
  n.na <- apply(is.na(mat), 2, sum)
  nx <- ncol(mat)
  up.lim <- rep(NA, nx)
  low.lim <- rep(NA, nx)
  stddev <- rep(NA, nx)
  for (i in 1:nx) {
    if (n.na[i] > nreps * (1 - confidence)) {
      next
    }
    conf <- confidence * nreps/(nreps - n.na[i])
    pr <- 0.5 * (1 - conf)
    up.lim[i] <- stats::quantile(mat[, i], 1 - pr, na.rm = TRUE)
    low.lim[i] <- stats::quantile(mat[, i], pr, na.rm = TRUE)
    stddev[i] <- stats::sd(mat[, i], na.rm = TRUE)
  }
  ndx <- !is.na(up.lim)
  fit <- data.frame(x = x.out[ndx], y.fit = y.fit[ndx], up.lim = up.lim[ndx], 
                    low.lim = low.lim[ndx], stddev = stddev[ndx])
  fit.boot <- list(nreps = nreps, confidence = confidence, 
                   span = f$pars$span, degree = f$pars$degree, normalize = f$pars$normalize, 
                   family = f$pars$family, parametric = f$pars$parametric, 
                   surface = f$pars$surface, data = dat, fit = fit)
  class(fit.boot) <- "loess.boot"
  return(fit.boot)
}


### Functions for bootstrapping confidence intervals with loess
# function to split df into list of groups, but preserve names in lists
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}

#### Identify the optimal 'span' parameter, using k-fold cross validation
## Function for conducting a k-fold cross validation to identify optimal Span
## parameter for the loess function. Function returns a dataframe with mean
## SSRs by span by majority group
optimize_span <- function(data, x, y, k = 10, span_seq = seq(.2,.8, by = .01), seed = 1) {
  set.seed(seed)
  #create k x length(span_seq) matrix to store sum of squared residuals (SSR) values
  cv_ssr_mtrx <- matrix(rep(x = NA, times = k * length(span_seq)), 
                        nrow = length(span_seq), ncol = k)
  
  #create df to store mean SSRs by span value
  SSRs <- data.frame("span" = span_seq)
  
  for (group in unique(data$majority_status)) {
    
    group_df <- data[data$majority_status== group, c(x,y)]
    folds <- sample(x = 1:k, size = nrow(group_df), replace = TRUE) #create k samples
    
    for(i in 1:length(span_seq)) {
      
      for (j in 1:k) {
        
        #fit model
        fmla <- as.formula(paste0(y," ~ ", x))
        loess_fit <- loess(formula = fmla, data = group_df[folds!= j,], span = span_seq[i], degree = 1)
        #predict out-of-sample and compute SSRs
        preds <- predict(object = loess_fit, newdata = group_df[folds == j, ])
        cv_ssr_mtrx[i,j] <- sum((group_df[folds == j, y] - preds)^2, na.rm = TRUE)
        SSRs[, group] <- rowMeans(cv_ssr_mtrx)
        
      }
      message(paste0("finished: ", i, " of ", length(span_seq)))
      flush.console()
    }
  }
  
  SSRs
}

choose_best_span <- function(SSRs, span_seq) {
  best_spans <- SSRs %>%
    summarise(across(colnames(SSRs)[-1], ~ span_seq[which.min(.x)])) %>%
    pivot_longer(colnames(ssr_income)[-1], names_to = 'majority_status', values_to = 'span')
  best_spans
}

plot_span_optimization <- function(SSRs) {
  
  SSRs_long <- SSRs %>%
    pivot_longer(colnames(SSRs)[-1], names_to = 'majority_status', values_to = 'mean_SSR')
  
  span_plot <- ggplot(data = SSRs_long, aes(x = span, y = mean_SSR, group = majority_status)) + #change y to 
    geom_point() +
    geom_smooth(method = 'loess') +
    facet_wrap(~ as.factor(majority_status), ncol = 3, scales = "free_y") +
    ggtitle("Span optimization for median income")
  span_plot
}

########### Function for fitting Loess with bootstrapped 90% CIs, using optimized span values
bootstrap_loess <- function(data, x, y, best_span, nreps, ci = .90) {
  
  list_of_dfs <- list()
  
  for (group in unique(data$majority_status)) {
    
    group_df <- data[data$majority_status== group, c(x,y)]
    
    loess_boot <- loess.boot(x = group_df[[x]], y = group_df[[y]], 
                                         nreps = nreps, confidence = ci, 
                                         span =best_span[best_span$majority_status == group, 'span'], 
                                         degree = 1)$fit
    
    list_of_dfs[[group]] <- data.frame(x = loess_boot$x, "ci_lower" = loess_boot$low.lim,
                                       "ci_upper" = loess_boot$up.lim, "predicted" = loess_boot$y.fit)
    message(paste0("finished: ", group, " of ", length(unique(combined_sunter_sample$majority_status))))
    flush.console()
  }
  
  boot_df <-list_of_dfs %>%
    bind_rows(.id = "majority_status") %>%
    remove_rownames()
  colnames(boot_df) <- c("majority_status", x, "ci_lower", "ci_upper", "predicted")
  boot_df
}

##### Function to create a DF with normalized predicted values normalized by no_majority predictions
##### Interpolates predictions to the same x-values for all majority groups to make plotting easier
get_normalized_df <- function(loess_df, x_var) {
  x_out <- loess_df[[x_var]][loess_df$majority_status =='no_majority']
  
  no_majority_predicted <- loess_df$predicted[loess_df$majority_status =='no_majority']
  no_majority_ci_upper <- loess_df$ci_upper[loess_df$majority_status =='no_majority']
  no_majority_ci_lower <- loess_df$ci_lower[loess_df$majority_status =='no_majority']
  
  
  list_of_dfs <- list()
  
  for (group in unique(loess_df$majority_status)) {
    normalized_df <- data.frame(x = x_out)
    colnames(normalized_df) <- x_var
    
    normalized_df$predicted <- approx(loess_df[[x_var]][loess_df$majority_status ==group], 
                                      loess_df$predicted[loess_df$majority_status ==group], 
                                      xout = x_out)$y
    normalized_df$ci_upper <- approx(loess_df[[x_var]][loess_df$majority_status ==group], 
                                     loess_df$ci_upper[loess_df$majority_status ==group], 
                                     xout = x_out)$y
    normalized_df$ci_lower <- approx(loess_df[[x_var]][loess_df$majority_status ==group], 
                                     loess_df$ci_lower[loess_df$majority_status ==group], 
                                     xout = x_out)$y
    
    normalized_df$predicted_std <- (normalized_df$predicted/no_majority_predicted - 1)*100
    normalized_df$ci_upper_std <- (normalized_df$ci_upper/no_majority_predicted - 1)*100
    normalized_df$ci_lower_std <- (normalized_df$ci_lower/no_majority_predicted - 1)*100
    
    list_of_dfs[[group]] <- normalized_df[, c(x_var, 'predicted_std', 'ci_lower_std', 'ci_upper_std')]
  }
  
  normed_df <-list_of_dfs %>%
    bind_rows(.id = "majority_status") %>%
    remove_rownames()
  #colnames(boot_df) <- c("majority_status", x_var, "ci_lower", "ci_upper", "predicted")
  normed_df
}


# catch errors when running loess by state
error_catch <- function(expr){
  tryCatch(expr,
           error = function(e){
             message("An error occurred:\n", e)
           },
           warning = function(w){
             message("A warning occured:\n", w)
           },
           finally = {
             message("Finally done!")
           })
}

# fit loess by state
loess_way <- function(state_df, x_var) {
  predict<- c("mean" = mean(state_df[[x_var]]), 
              quantile(state_df[[x_var]], c(.25,.5, .75, .9)))
  
  state_stats <- state_df %>%
    summarize(n_tracts = n(),
              total_installs_state = sum(existing_installs_count),
              installs_rate_by_tract = total_installs_state/ n_tracts,
              total_buildings = sum(count_qualified),
              rate_per_1000_buildings = (total_installs_state/total_buildings)*1000,
              state_ave = unique(state_ave)
    )
  
  list_of_dfs <- list()
  for (group in unique(state_df$majority_status)) {
    list_of_dfs[[group]] <- data.frame(predict)
    colnames(list_of_dfs[[group]]) <- c(x_var)
    
    group_df <- state_df[state_df$majority_status== group, c("normed_SunroofDeployment","median_income")]
    error_catch ( {
      loess_fit <- loess(formula = normed_SunroofDeployment ~ median_income, data = group_df, span = .8, degree = 1)
      preds <- predict(object = loess_fit, newdata = list_of_dfs[[group]], se = TRUE)
      
      list_of_dfs[[group]]['stat'] = c('mean', 'q25', 'q50', 'q75', 'q90')
      list_of_dfs[[group]]['predicted'] = preds[['fit']]
      list_of_dfs[[group]]['ci_lower'] = preds[['fit']]-1.96*preds[['se.fit']]
      list_of_dfs[[group]]['ci_upper'] = preds[['fit']]+1.96*preds[['se.fit']]
      list_of_dfs[[group]]['n_group_tracts'] = rep(nrow(group_df), length(predict))
      list_of_dfs[[group]]['n_tracts'] = rep(state_stats$n_tracts, length(predict))
      list_of_dfs[[group]]['installs_rate_by_tract'] = rep(state_stats$installs_rate_by_tract, length(predict))
      list_of_dfs[[group]]['rate_per_1000_buildings'] = rep(state_stats$rate_per_1000_buildings, length(predict))
      list_of_dfs[[group]]['state_ave'] = rep(state_stats$state_ave, length(predict))
      
    })
  }
  
  loess_df <-list_of_dfs %>%
    bind_rows(.id = "majority_status") %>%
    remove_rownames()
  loess_df
}

scaleFUN <- function(x) sprintf("%.1f", x) #prints 1 decimal point for scales

########################## End Functions ##########################


################### ################### ################### ################### 
################### Replications of Sunter figures
################### ################### ################### ################### 
sample_filter_count_200 <-readRDS("data/sample_filter_count_200.rds")
combined_sunter_sample <- readRDS("data/combined_sunter_sample.rds")

list_of_samples <- list(sample_filter_count_200, combined_sunter_sample)


############# WARNING: THE FOLLOWING LOOP TAKES A LOOOOONG TIME TO RUN!
############# For convenience, YOU can just skip this loop and load pre-run 
############# versions of the resulting datasets from "results" folder.
############# By default, the next loop (line 325) will read the pre-run datasets.

### For each sample:
### (1) find the optimal span parameter for each group,
### (2) fit LOESS with bootstrapped CI for each group
### (3) Save resulting results datasets

for (i in seq(length(list_of_samples)) ) {
  
  analytic_sample <- as.data.frame(list_of_samples[[i]])  
  #re-order levels for plotting
  analytic_sample$majority_status <- factor(analytic_sample$majority_status, 
                                                 levels = c('no_majority', 'asian', 'black', 'hisp', 'white'))
  
  #### Create data for plotting (using optimal span and bootstrap)
  span_seq <- seq(.2,.8, by = .01) #range of span values to test
  seed = 1 # replicate results
  k <- 10 #k for k-fold cross validation
  nreps = 10000
  
  ssr_income <- optimize_span(analytic_sample, x = "median_income", y = "normed_SunroofDeployment", k = k, span_seq = span_seq, seed = seed)
  best_span_income <- choose_best_span(ssr_income, span_seq)
  saveRDS(best_span_income, file = paste0("results/best_span_income_", i,".rds"))
  
  loess_median_income <- bootstrap_loess(data = analytic_sample, x = "median_income", y= "normed_SunroofDeployment", best_span = best_span_income, nreps = nreps, ci = .9)
  saveRDS(loess_median_income, file = paste0("results/loess_median_income_", i,".rds"))
  
  
  ssr_perc_renter <- optimize_span(analytic_sample, x = "perc_renter", y = "normed_SunroofDeployment", k = k, span_seq = span_seq, seed = seed)
  best_span_perc_renter <- choose_best_span(ssr_perc_renter, span_seq)
  saveRDS(best_span_perc_renter, file = paste0("results/best_span_perc_renter_", i,".rds"))
  
  loess_perc_renter <- bootstrap_loess(data = analytic_sample, x = "perc_renter", y= "normed_SunroofDeployment", best_span = best_span_perc_renter, nreps = nreps, ci = .9)
  saveRDS(loess_perc_renter, file = paste0("results/loess_perc_renter", i,".rds"))
  
}


##### START HERE IF SKIPPING THE LOOOOOONG LOOP ABOVE
## Create replication figures for each datasest, using results generated above (reads pre-run results by default)
for (i in seq(length(list_of_samples)) ) {
    
    loess_median_income <- readRDS(file = paste0("results/loess_median_income_", i,".rds"))
    loess_perc_renter <- readRDS(file = paste0("results/loess_perc_renter", i,".rds"))
    
    x_axis_range_income <- c(22000, max(loess_median_income$median_income))
    x_axis_range_renter <- c(0, 100)
    
    analytic_sample <- as.data.frame(list_of_samples[[i]])
    
    ## Replication of figure 2(a)
    fig2a <- ggplot(analytic_sample, aes(median_income)) + 
      geom_histogram(aes(fill=majority_status), 
                     breaks = seq(from=20000, to=255000, by=5000),
                     include.lowest = TRUE) +
      scale_x_log10(breaks = c(10000, 25000, 50000,75000,100000,150000,200000, 250000)) +
      scale_y_continuous(breaks = c(2000, 4000), limits = c(0, 4000)) +
      scale_fill_manual(name = "Majority in tract", 
                        labels = c("No majority", "Asian","Black", "Hispanic", "White"),
                        values=c("seagreen4", 'magenta4', 'grey30','red', 'cornflowerblue')) +
      ylab("Tracts") +
      xlab("Median household income") +
      
      coord_cartesian( ylim = c(0, 4000), xlim = x_axis_range_income) +
      
      theme_classic() +
      theme( axis.text.x=element_blank(),
             axis.title.x=element_blank(),
             axis.text.y=element_text(size=7, colour = "black"),
             axis.title.y=element_text(size=6, colour = "black"),
             legend.position = "none",
             panel.border = element_rect(fill = NA),
             plot.margin = margin(10,0,0,10, "pt")  # if combining figs 2 and 3
             
      )
    print(fig2a)

    ## Replication of figure 2(b)
    fig2b <- ggplot(loess_median_income, aes(x = median_income, y = predicted, colour = majority_status)) +
      geom_line(size = .5) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = majority_status), alpha = .20, colour = NA) +
      
      scale_color_manual(name = "Majority", 
                         labels = c("Asian","Black", "Hispanic", "No majority", "White"),
                         values=c('magenta4', 'grey30','red', "seagreen4", 'cornflowerblue')) + 
      scale_fill_manual(name = "Majority", 
                        labels = c("Asian","Black", "Hispanic", "No majority", "White"),
                        values=c('magenta4', 'grey30','red', "seagreen4", 'cornflowerblue')) +
      scale_x_log10(breaks = c(10000, 25000, 50000,75000,100000,150000,200000, 250000)) +
      scale_y_continuous(labels = scaleFUN) +
      
      coord_cartesian( ylim = c(0,4.5), xlim = x_axis_range_income) +
      
      ylab("State-normalized\nsolar deployment") +
      xlab("Median household income") +
      theme_classic() +
      theme( axis.text.y=element_text(size=7, colour = "black"),
             axis.title.y=element_text(size=6, colour = "black"),
             axis.text.x=element_blank(),
             axis.title.x=element_blank(),
             legend.title = element_blank(),
             legend.text = element_text(size =5, colour = "black"),
             legend.key.size = unit(2.5, "mm"),
             legend.spacing.y = unit(1, 'mm'),
             legend.position = c(.1,.7),
             legend.box.background = element_rect(fill = "transparent", colour = NA),
             legend.background = element_rect(fill = "transparent", colour = NA),
             legend.key = element_rect(fill = "transparent", colour = NA),
             panel.border = element_rect(fill = NA),
             plot.margin = margin(0,0,0,10, "pt")  # if combining figs 2 and 3
             
      )
    print(fig2b)
    
    ## standardize by no_majority
    normed_loess_median_income <- get_normalized_df(loess_median_income, "median_income")
    
    ## Replication of figure 2(c)
    fig2c <- ggplot(normed_loess_median_income, aes(x = median_income, y = predicted_std, colour = majority_status)) +
      geom_line(size = .5) +
      geom_ribbon(aes(ymin = ci_lower_std, ymax = ci_upper_std, fill = majority_status), alpha = .20, colour = NA) +
      
      scale_color_manual(name = "Majority", 
                         labels = c("Asian","Black", "Hispanic", "No majority", "White"),
                         values=c('magenta4', 'grey30','red', "seagreen4", 'cornflowerblue')) + 
      scale_fill_manual(name = "Majority", 
                        labels = c("Asian","Black", "Hispanic", "No majority", "White"),
                        values=c('magenta4', 'grey30','red', "seagreen4", 'cornflowerblue')) +
      scale_x_log10(breaks = c(10000, 25000, 50000,75000,100000,150000,200000, 250000)) +
      scale_y_continuous( breaks = seq(from=-150, to =150, by = 50)) +
      
      coord_cartesian( ylim = c(-150,150), xlim = x_axis_range_income) +
      
      ylab("Solar deployment relative\nto no majority census tracts") +
      xlab("Median household income (2013 US$)") +
      theme_classic() +
      theme( axis.text.x=element_text(size=7, colour = "black", angle = 20, vjust= .5),
             axis.text.y=element_text(size=7, colour = "black"),
             axis.title.y=element_text(size=6, colour = "black"),
             axis.title.x=element_text(size=7, colour = "black"),
             legend.position = "none",
             panel.border = element_rect(fill = NA),  
             plot.margin = margin(0,0,10,0, "pt") # if combining figs 2 and 3
             
      )
    
    print(fig2c)
  
    ## Combine components of Fig 2 and save
    fig2_combined <- ggarrange(fig2a, fig2b, fig2c,
                               labels = c("a.", "b.", "c."), heights = c(.4, .75, 1.1), align = "v",
                               ncol = 1, nrow = 3, hjust = -.2,
                               font.label = list(size = 7))
    print(fig2_combined)
    ggsave(paste0("results/fig2_", i,".pdf"), fig2_combined, width=100, height=100, units="mm", dpi = 300)
    
    
    ## Replication of figure 3(a)
    fig3a <- ggplot(analytic_sample, aes(perc_renter)) + 
      geom_histogram(aes(fill=majority_status), 
                     breaks = seq(from=0, to=100, by=5),
                     include.lowest = TRUE) +
      scale_x_continuous(breaks = seq(from=0, to= 100, by = 25)) +
      scale_y_continuous(breaks = c(2000, 4000), limits = c(0, 4000)) +
      scale_fill_manual(name = "Majority in tract", 
                        labels = c("No majority", "Asian","Black", "Hispanic", "White"),
                        values=c("seagreen4", 'magenta4', 'grey30','red', 'cornflowerblue')) +
      ylab("Tracts") +
      xlab("Percentage of households occupied by renters") +
      
      coord_cartesian( ylim = c(0, 4000), xlim = x_axis_range_renter) +
      
      theme_classic() +
      theme( axis.text.x=element_blank(),
             axis.text.y=element_text(size=7, colour = "black"),
             axis.title.y=element_text(size=6, colour = "black"),
             axis.title.x=element_blank(),
             legend.position = 'none',
             panel.border = element_rect(fill = NA),
             plot.margin = margin(10,10,0,0, "pt")  # if combining figs 2 and 3
      )
    
    ## Replication of figure 3(b)
    fig3b <- ggplot(loess_perc_renter, aes(x = perc_renter, y = predicted, colour = majority_status)) +
      geom_line(size = .5) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = majority_status), alpha = .20, colour = NA) +
      
      scale_color_manual(name = "Majority", 
                         labels = c("Asian","Black", "Hispanic", "No majority", "White"),
                         values=c('magenta4', 'grey30','red', "seagreen4", 'cornflowerblue')) + 
      scale_fill_manual(name = "Majority", 
                        labels = c("Asian","Black", "Hispanic", "No majority", "White"),
                        values=c('magenta4', 'grey30','red', "seagreen4", 'cornflowerblue')) +
      scale_y_continuous(labels = scaleFUN) +
      
      coord_cartesian( ylim = c(0,2.5), xlim = x_axis_range_renter) +
      
      ylab("State-normalized\nsolar deployment") +
      xlab("Percentage of households occupied by renters") +
      theme_classic() +
      theme( axis.text.x=element_blank(),
             axis.text.y=element_text(size=7, colour = "black"),
             axis.title.y=element_text(size=6, colour = "black"),
             axis.title.x=element_blank(),
             legend.title = element_blank(),
             legend.text = element_text(size =5, colour = "black"),
             legend.key.size = unit(2.5, "mm"),
             legend.spacing.y = unit(1, 'mm'),
             legend.position = c(.8,.7),
             legend.box.background = element_rect(fill = "transparent", colour = NA),
             legend.background = element_rect(fill = "transparent", colour = NA),
             legend.key = element_rect(fill = "transparent", colour = NA),
             panel.border = element_rect(fill = NA),
             plot.margin = margin(0,10,0,0, "pt") # if combining figs 2 and 3
      )
    print(fig3b)

    ## standardize by no_majority
    normed_loess_perc_renter <- get_normalized_df(loess_perc_renter, "perc_renter")
    
    
    ## Replication of figure 2(a)
    fig3c <- ggplot(normed_loess_perc_renter, aes(x = perc_renter, y = predicted_std, colour = majority_status)) +
      geom_line(size = .5) +
      geom_ribbon(aes(ymin = ci_lower_std, ymax = ci_upper_std, fill = majority_status), alpha = .20, colour = NA) +
      
      scale_color_manual(name = "Majority", 
                         labels = c("Asian","Black", "Hispanic", "No majority", "White"),
                         values=c('magenta4', 'grey30','red', "seagreen4", 'cornflowerblue')) + 
      scale_fill_manual(name = "Majority", 
                        labels = c("Asian","Black", "Hispanic", "No majority", "White"),
                        values=c('magenta4', 'grey30','red', "seagreen4", 'cornflowerblue')) +
      scale_y_continuous( breaks = seq(from=-150, to =150, by = 50)) +
      
      coord_cartesian( ylim = c(-100,75), xlim = x_axis_range_renter) +
      
      ylab("Solar deployment relative\nto no majority census tracts") +
      xlab("Percentage of households occupied by renters") +
      theme_classic() +
      theme( axis.text.x=element_text(size=7, colour = "black"),
             axis.text.y=element_text(size=7, colour = "black"),
             axis.title.y=element_text(size=6, colour = "black"),
             axis.title.x=element_text(size=7, colour = "black"),
             legend.position = 'none',
             panel.border = element_rect(fill = NA),
             plot.margin = margin(0,10,10,0, "pt") # if combining figs 2 and 3
      )
    print(fig3c)
    
    ## Combine components of Fig 2 and save
    fig3_combined <- ggarrange(fig3a, fig3b, fig3c,
                               labels = c("a.", "b.", "c."), heights = c(.4, .75, 1.1), align = "v",
                               ncol = 1, nrow = 3, hjust = -.2,
                               font.label = list(size = 7))
    print(fig3_combined)
    ggsave(paste0("results/fig3_", i,".pdf"), fig3_combined, width=100, height=100, units="mm", dpi = 300)
    
    ## Combine figures 2 and 3 for publication
    fig2_3_combined <- ggarrange(fig2a, fig3a,fig2b, fig3b, fig2c, fig3c,
                                 labels = c("a.", "d.", "b.", "e.", "c.", "f."), 
                                 heights = c(.4, .75, 1.1),
                                 ncol = 2, nrow = 3, 
                                 #hjust = -.2,
                                 font.label = list(size = 8),
                                 align = 'v')
    print(fig2_3_combined)
    
    ggsave(paste0("results/fig2_3", i,".pdf"), fig2_3_combined, width=200, height=100, units="mm", dpi = 300)
    
    
    ################################
    ################################
    
    ########## Fig 4 replication
    seeded_df <- analytic_sample %>%
      group_by(majority_status) %>%
      summarize(
        n = n(),
        seeded = (sum(SunroofSeeded, na.rm = TRUE)/n())*100,
        unseeded = 100-seeded,
        
      ) %>%
      pivot_longer(cols = c(seeded, unseeded),
                   names_to = "seeded_status_sunroof",
                   values_to = "share_sunroof") %>%
      filter(majority_status != "no_majority")
    
    seeded_df$seeded_status_sunroof <- factor(seeded_df$seeded_status_sunroof, levels = c("unseeded", "seeded"))
    
    ## re-order levels for plotting
    seeded_df$majority_status <- factor(seeded_df$majority_status,levels = c("white", "asian", "hisp", "black"))
    
    fig4 <- ggplot(seeded_df, aes(x= majority_status, y = share_sunroof, fill = seeded_status_sunroof, color = majority_status)) +
      geom_bar(position = 'stack', stat='identity') +
      geom_label(aes(label=round(share_sunroof, digits = 0)), position = position_stack(vjust = .5), fill = 'white', size = 3) +
      scale_x_discrete( labels = c("White", "Asian", "Hispanic", "Black")) +
      scale_y_continuous(breaks = seq(from = 0.0, to = 100, by = 20)) +
      scale_fill_manual(labels = c("No installations", "Existing installations"),
                        values=c('white', 'black')) +
      scale_color_manual(
        labels = c("White","Asian","Hispanic", "Black"),
        values=c('cornflowerblue','magenta4', 'red', 'grey30')) +   
      
      guides(size = "legend", colour = "none") +
      ylab("Percentage") +
      coord_flip() +
      theme_classic() +
      
      theme( axis.text.x=element_text(size=7, colour = "black"),
             axis.text.y=element_text(size=7, colour = "black"),
             axis.title.y=element_blank(),
             axis.title.x=element_text(size=7, colour = "black"),
             legend.title = element_blank(),
             legend.text = element_text(size =7, colour = "black"),
             legend.key.size = unit(.5, "cm"),
             legend.spacing.y = unit(.2, 'cm'),
             legend.position = 'top',
             legend.key = element_rect(color = "black")
      )
    print(fig4)
    ggsave(paste0("results/rep_fig4_", i,".pdf"), fig4, width=150, height=100, units="mm", dpi = 300)
    
    #Average relative differences among groups
    ave_diffs_income <- normed_loess_median_income %>%
      group_by(majority_status) %>%
      summarize(relative_diff_no_majority = mean(predicted_std, na.rm = TRUE)
      )
    saveRDS(ave_diffs_income, file = paste0("results/ave_diffs_income_", i,".rds"))
    
    ave_diffs_perc_renter <- normed_loess_perc_renter %>%
      group_by(majority_status) %>%
      summarize(relative_diff_no_majority = mean(predicted_std, na.rm = TRUE)
      )
    saveRDS(ave_diffs_perc_renter, file = paste0("results/ave_diffs_perc_renter_", i,".rds"))
  
  ############## ############## ############## ############## 
  ##############  Multivariable models predicting seeded tracts 
  ############## ############## ############## ############## 
  
  analytic_sample$majority_status <- factor(analytic_sample $majority_status, 
                                                    levels = c('no_majority', 'asian', 'black', 'hisp', 'white'))
  analytic_sample$state_name <- factor(analytic_sample$state_name)
  analytic_sample <- as.data.frame(lapply(analytic_sample, function(x) if(is.factor(x)) factor(x) else x)) #eliminates unused factor levels
  
  #simple linear model
  seeded_lm_1 <- lm(SunroofSeeded ~ majority_status + median_income  + perc_renter + factor(state_name),
                    data = analytic_sample)
  summary(seeded_lm_1)
  
  #logistic regression
  logistic_seeded_lm_1 <- glm(SunroofSeeded ~ majority_status + median_income  + perc_renter + state_name,
                              data = analytic_sample, family = "binomial")
  summary(logistic_seeded_lm_1)
  
  ## LPM with smooths for median income and perc_renter
  gam_seeded_lm_1 <- gam(
    SunroofSeeded ~ 
      1 +
      majority_status + 
      s(median_income) + 
      s(perc_renter) + 
      factor(state_name),
    
    data = analytic_sample,
    method = "REML"
  )
  summary(gam_seeded_lm_1)
  
  stargazer(gam_seeded_lm_1, type = "html", style="apsr", 
            title = "Linear probability model predicting existence of 1 or more rooftop PV installations in a census tract",
            out = paste0("results/seeded_gam_", i,".html"),
            star.cutoffs = c(.05, .01),
            covariate.labels = c('Asian', "Black", "Hispanic", "White", "Intercept"),
            keep.stat = c("n", 'adj.rsq'),
            header=FALSE, dep.var.labels="Seeded",keep=c(1:4,73)
  )
  
  ## logistic regression with smooths for median income and perc_renter
  logistic_gam_seeded_lm_2 <- gam(
    SunroofSeeded ~ 
      majority_status + 
      s(median_income) + 
      s(median_income, by = majority_status)+
      s(perc_renter) + 
      s(perc_renter, by = majority_status) +
      factor(state_name),
    family = binomial, 
    data = analytic_sample,
    method = "REML"
  )
  summary(logistic_gam_seeded_lm_2)
  
  ########################## ########################## ########################## 
  ########################## estimates by state 
  
  state_results_list <- list()
  for (state in unique(analytic_sample$state_name) ) { #skipped missouri (didn't converge)
    
    state_df <- analytic_sample[analytic_sample$state_name == state,]
    if (nrow(state_df) > 100 ) {
      
      # reorder factor levels for majority_status
      state_df$majority_status <- factor(state_df$majority_status, 
                                         levels = c('no_majority', 'asian', 'black', 'hisp', 'white'))
      state_df$majority_status <- factor(state_df$majority_status)
      state_df[] <- lapply(state_df, function(x) if(is.factor(x)) factor(x) else x) #eliminates unused factor levels
      
      state_results_list[[state]] <- loess_way(state_df, "median_income")
    }
    else {
      NULL
    }
    
    message(paste0("finished: ", state))
    
  }
  
  
  states_df <-state_results_list %>%
    bind_rows(.id = "state_name") %>%
    remove_rownames() %>%
    na.omit %>%
    left_join(unique(analytic_sample[,c("state_name", "state_rank", "state_installs_total")]), by = "state_name", keep = FALSE)
  
  
  states_df <- states_df %>%
    group_by(majority_status) %>%
    mutate(above_ave = ifelse(ci_lower > 1, 1, ifelse(ci_upper<1, -1,0)),
           ave_count_by_tract = state_installs_total/n_tracts)
  summary_states_df <- states_df %>%
    group_by(majority_status, above_ave, stat) %>%
    summarize(n_states = n(),
              n_tracts = sum(n_tracts),
              ave_count = mean(state_installs_total),
              median_count_tract_level = median(analytic_sample$existing_installs_count[analytic_sample$state_name %in% states_df$state_name[states_df$majority_status == majority_status & states_df$above_ave == above_ave] ]),
              q_25_tract_level = quantile(analytic_sample$existing_installs_count[analytic_sample$state_name %in% states_df$state_name[states_df$majority_status == majority_status & states_df$above_ave == above_ave] ], probs = c(.25)),
              q_90_tract_level = quantile(analytic_sample$existing_installs_count[analytic_sample$state_name %in% states_df$state_name[states_df$majority_status == majority_status & states_df$above_ave == above_ave] ], probs = c(.90)),
              count_per_tract = sum(state_installs_total)/n_tracts,
              median_count_state_level = median(state_installs_total))
  
  
  states_df$majority_status <- factor(states_df$majority_status, levels = c("asian","black","hisp" ,"no_majority","white"), 
                                      labels = c("Asian", "Black", "Hispanic", "No majority", 'White'))
  
  results_by_state_by_group <- ggplot(states_df[states_df$stat == 'mean',], aes(x = predicted, y = state_rank, label = state_name)) +
    geom_point(aes(size = state_installs_total, color = factor(above_ave)), alpha = .5) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_color_manual(values = c("blue", "grey30", "red"), labels = c("Above state\nmean", "Diff. not\nsignificant", "Below state\nmean")) +
    scale_size( name = "Legend",
                breaks = c(1000, 50000, 300000),
                labels = c("1,000", "50,000", "300,000"),
                range = c(.5, 10),
                trans = "identity"
    ) +
    scale_y_continuous(limits = c(1,51), breaks = c(seq(2, 47, by = 5), 51), labels = rev(c(1,seq(5, 50, by = 5))), name = "State rank by average solar deployment") +
    scale_x_continuous(name = "State normalized solar deployment for a census tract with median income held at state mean") +
    geom_text_repel(label = ifelse(states_df[states_df$stat == 'mean',]$above_ave != 0,states_df[states_df$stat == 'mean',]$state_name, ""), size = 1.5) +
    #geom_text(label = ifelse(states_df[states_df$stat == 'mean',]$above_ave != 0,states_df[states_df$stat == 'mean',]$state_name, ""), size = 1.5, position = position_jitter(.4)) +
    facet_wrap( ~majority_status, nrow = 5, scales = "fixed") +
    theme_classic() +
    theme( panel.border = element_rect(fill = NA),
           
           axis.text.x=element_blank(),
           axis.text.y=element_text(size=7, colour = "black"),
           axis.title.y=element_text(size=7, colour = "black"),
           axis.title.x=element_blank(),
           
           #legend
           legend.title = element_blank(),
           legend.margin = margin(t = -.5, r=1, b = 1, l = 0, unit = 'mm'),
           legend.text = element_text(size =6, colour = "black"),
           legend.spacing.y = unit(1, 'mm'),
           legend.position = c(.26,.92),
           legend.direction = "horizontal",
           legend.box = "vertical",
           legend.box.background = element_rect(fill = "white", colour = "black"),
           legend.background = element_rect(fill = "transparent", colour = NA),
           legend.key = element_rect(fill = "transparent", colour = NA),
           legend.key.size = unit(.05, 'lines'),
           
           #facet titles
           strip.background =element_rect(fill="black"),
           strip.text = element_text(colour = 'white', size = 7),
           strip.text.x = element_text(margin = margin(.5,0,.5,0, "pt"))
           
    ) +
    guides(color = guide_legend(override.aes = list(size=3))) +
    coord_flip()
  print(results_by_state_by_group)
  
  
  cum_sum_plot <- list_of_samples[[1]] %>%
    group_by(state_name) %>%
    summarize(state_ave =  mean(SunroofDeployment, na.rm = TRUE),
              state_installs_total = sum(existing_installs_count)) %>%
    ungroup() %>%
    arrange(state_ave) %>%
    mutate(state_rank = row_number(),
           quartile = ntile(state_ave, 4),
           cum_sum_installs = cumsum(state_installs_total),
           cum_perc_installs = cumsum(state_installs_total)/sum(state_installs_total)*100
    ) %>%
    ggplot(aes(x = state_rank, y = cum_perc_installs)) +
    geom_line(size = .5) +
    scale_x_continuous(limits = c(1,51), breaks = c(seq(2, 47, by = 5), 51), labels = rev(c(1,seq(5, 50, by = 5))), name = "State rank by average solar deployment") +
    scale_y_continuous(limits = c(0,100), breaks = c(seq(0, 100, by = 25)), labels = c(seq(0, 100, by = 25)), name = "Cumulative %\nofsolar installations") +
    theme_classic() +
    theme( panel.border = element_rect(fill = NA),
           
           axis.text.x=element_text(size=7, colour = "black"),
           axis.text.y=element_text(size=7, colour = "black"),
           axis.title.y=element_text(size=7, colour = "black"),
           axis.title.x=element_text(size=7, colour = "black")
    )
  print(cum_sum_plot)
  
  state_results_combined <- ggarrange(results_by_state_by_group, cum_sum_plot,
                                      labels = c("a.", "b."), heights = c( 1, .25), align = "v",
                                      ncol = 1, nrow = 2, hjust = -.2, font.label = list(size= 7))
  
  print(state_results_combined)
  
  ggsave(paste0("results/results_by_state_by_group", i,".pdf"), state_results_combined, width=125, height=150, units="mm", dpi = 300)

}
  
  