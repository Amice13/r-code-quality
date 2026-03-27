#############################################################################################
# Code to create TDR or FAR plots to visualize simulation study results
# Code is also provided to create plots of a single realization of each Phase II scenario
# in the simulation study
# Taylor Grimm
# September 27, 2024
#############################################################################################

library(MASS)
library(tidyverse)

load('all_tdr_far_independent_005.rds')
load('sst2_tdr_far_independent_005.rds')

all_tdr_far_independent_005 <- cbind(all_tdr_far_independent_005, sst2_tdr_far_independent_005[, 6:7])

make_plot <- function(dim = 3, metric_val = 'tdr', scenario = 1) {
  
  far_vals <- all_tdr_far_independent_005 |>
    # filter(dimension == dim, ph2_scenario == scenario) |>
    filter(dimension == dim, ph2_scenario == scenario, contamination %in% c(0, 0.2, 0.4)) |>
    select(m0, shift, contamination, contains('far')) |> 
    rename_with(~ str_remove(.x, paste0('_', 'far')), .cols = contains('far')) |> 
    pivot_longer(cols = 4:11)
  
  if(scenario == 1) {
    data_plot <- all_tdr_far_independent_005 |>
      # filter(dimension == dim, ph2_scenario == scenario) |>
      filter(dimension == dim, ph2_scenario == scenario, contamination %in% c(0, 0.2, 0.4)) |>
      select(m0, shift, contamination, contains(metric_val)) |> 
      rename_with(~ str_remove(.x, paste0('_', metric_val)), .cols = contains(metric_val)) |> 
      pivot_longer(cols = 4:11, values_to = 'tdr')
  } else {
    data_plot <- all_tdr_far_independent_005 |>
      # filter(dimension == dim, ph2_scenario == scenario) |>
      filter(dimension == dim, ph2_scenario == scenario, contamination %in% c(0, 0.2, 0.4)) |>
      select(m0, shift, contamination, contains(metric_val)) |> 
      rename_with(~ str_remove(.x, paste0('_', metric_val)), .cols = contains(metric_val)) |> 
      pivot_longer(cols = 4:11, values_to = 'tdr') |> 
      mutate(far = far_vals$value,
             large_far = far > 0.05,
             alpha_val = ifelse(large_far, 0.9, 1))
  }
  data_plot <- data_plot |>
    mutate(Method = factor(name, levels = c("mewma_classical", "mewma_robust",
                                            "t2_classical", "t2_robust",
                                            "ssmewma", "sst2",
                                            "robust_bayes_mewma", "robust_bayes_t2"),
                           labels = c("MEWMA", "R-MEWMA",
                                      "italic(T)^2", "R-italic(T)^2",
                                      "SS-MEWMA", "SS-italic(T)^2",
                                      "RSSB-MEWMA", "RSSB-italic(T)^2")))
  # labels = c("MEWMA", "MEWMA[RMCD]",
  #            "italic(T)^2", "italic(T)[RMCD]^2",
  #            "SS-MEWMA", "SS-italic(T)^2",
  #            "RSSB-MEWMA", "RSSB-italic(T)^2")))
  
  line_colors <- c("MEWMA" = viridis::viridis(5, option = 'H')[1],
                   "R-MEWMA" = viridis::viridis(5, option = 'H')[1],
                   "italic(T)^2" = viridis::viridis(5, option = 'H')[2],
                   "R-italic(T)^2" = viridis::viridis(5, option = 'H')[2],
                   "RSSB-MEWMA" = 'forestgreen',
                   "RSSB-italic(T)^2" = 'purple2',
                   "SS-MEWMA" = viridis::viridis(5, option = 'H')[5],
                   "SS-italic(T)^2" = viridis::viridis(5, option = 'H')[4])
  line_types <- c("MEWMA" = 1,
                  "R-MEWMA" = 1,#2,
                  "italic(T)^2" = 1,
                  "R-italic(T)^2" = 1,#2,
                  "RSSB-MEWMA" = 1,#2,
                  "RSSB-italic(T)^2" = 1,#2,
                  "SS-MEWMA" = 1,
                  "SS-italic(T)^2" = 1)
  point_types <- c("MEWMA" = 19,
                   "R-MEWMA" = 17,
                   "italic(T)^2" = 19,
                   "R-italic(T)^2" = 17,
                   "RSSB-MEWMA" = 17,
                   "RSSB-italic(T)^2" = 17,
                   "SS-MEWMA" = 19,
                   "SS-italic(T)^2" = 19)
  ylabel <- case_when(metric_val == 'tdr' ~ 'Average True Detection Rate',
                      metric_val == 'far' ~ 'Average False Alarm Rate')
  
  if(scenario == 1) {
    data_plot |> 
      ggplot(aes(shift, tdr, group = Method, col = Method, lty = Method, shape = Method)) +
      geom_line(lwd = .4) +
      geom_point(size = 1.1) +
      xlab(expression(delta)) +
      ylab(ylabel) +
      facet_grid(cols = vars(m0), rows = vars(contamination), labeller = label_parsed) +
      scale_linetype_manual(values = line_types, labels = scales::label_parse()) +
      scale_shape_manual(values = point_types, labels = scales::label_parse()) +
      scale_color_manual(values = line_colors, labels = scales::label_parse()) +
      scale_y_continuous(sec.axis = sec_axis(~., name = expression(pi), breaks = NULL, labels = NULL),
                         limits = c(0, 1)) +
      scale_x_continuous(sec.axis = sec_axis(~., name = expression(italic(m)[0]), breaks = NULL, labels = NULL)) +
      ggtitle(bquote("Performance Under Scenario"~.(scenario)*';'~italic(p)~'='~.(dim))) +
      theme_light() +
      theme(legend.text = element_text(size = 8),
            legend.title = element_text(size = 9)) +
      guides(color = guide_legend(override.aes = list(size = 1)),
             alpha = 'none') +
      theme(legend.margin = margin(0, 0, 0, 0),
            legend.spacing.x = unit(0, "mm"),
            legend.spacing.y = unit(0, "mm"))
  } else {
    data_plot |> 
      ggplot(aes(shift, tdr, group = Method, col = Method, lty = Method, shape = Method,
                 alpha = alpha_val)) +
      geom_line(lwd = .4) +
      geom_point(size = 1.1) +
      xlab(expression(delta)) +
      ylab(ylabel) +
      facet_grid(cols = vars(m0), rows = vars(contamination), labeller = label_parsed) +
      scale_linetype_manual(values = line_types, labels = scales::label_parse()) +
      scale_shape_manual(values = point_types, labels = scales::label_parse()) +
      scale_color_manual(values = line_colors, labels = scales::label_parse()) +
      scale_alpha(range = c(0.15, 1)) +
      scale_y_continuous(sec.axis = sec_axis(~., name = expression(pi), breaks = NULL, labels = NULL),
                         limits = c(0, 1)) +
      scale_x_continuous(sec.axis = sec_axis(~., name = expression(italic(m)[0]), breaks = NULL, labels = NULL)) +
      ggtitle(bquote("Performance Under Scenario"~.(scenario)*';'~italic(p)~'='~.(dim))) +
      theme_light() +
      theme(legend.text = element_text(size = 8),
            legend.title = element_text(size = 9)) +
      guides(color = guide_legend(override.aes = list(size = 1)),
             alpha = 'none') +
      theme(legend.margin = margin(0, 0, 0, 0),
            legend.spacing.x = unit(0, "mm"),
            legend.spacing.y = unit(0, "mm"))
  }
  
}

# Create and save plots of the TDR for scenarios 1-4
make_plot(dim = 3, metric_val = 'tdr', scenario = 1)
ggsave('scenario1_results.pdf', width = 7, height = 5)

make_plot(dim = 3, metric_val = 'tdr', scenario = 2)
ggsave('scenario2_results.pdf', width = 7, height = 5)

make_plot(dim = 3, metric_val = 'tdr', scenario = 3)
ggsave('scenario3_results.pdf', width = 7, height = 5)

make_plot(dim = 3, metric_val = 'tdr', scenario = 4)
ggsave('scenario4_results.pdf', width = 7, height = 5)


################################################################
###### Plots of Phase II scenarios #############################
################################################################
library(mvtnorm)

set.seed(24)

realization_plot <- function(m0 = 500, dimension = 3, contamination = 0.2, errors = 'mvn', shift = 1.5, ph2_scenario = 1) {
  gen_dat <- mvrnorm(n = m0, mu = rep(0, dimension), Sigma = diag(dimension))
  
  # define the initial "training" set as the first m0 observations
  # with contaminated errors (or without if contamination = 0)
  if(contamination > 0) {
    # determine how many and which observations should have their error terms replaced
    # (use ceiling() in case m0 * contamination is not a whole number)
    num_contaminated <- ceiling(m0 * contamination)
    contamination_indices <- sample(1:m0, size = num_contaminated)
    dat_train <- gen_dat
    dat_train[contamination_indices, ] <- mvrnorm(n = num_contaminated, mu = rep(shift, dimension), Sigma = diag(dimension))
  } else {
    dat_train <- gen_dat
  }
  if(ph2_scenario == 1) {
    dat <- mvrnorm(1000, mu = rep(shift, dimension), Sigma = diag(dimension))
    oc_indices <- 1:1000
    ic_indices <- NULL
  } else if(ph2_scenario == 2) {
    dat <- rbind(mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(800, mu = rep(shift, dimension), Sigma = diag(dimension)))
    oc_indices <- c(201:1000)
    ic_indices <- c(1:200)
  } else if(ph2_scenario == 3) {
    dat <- rbind(mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(0, dimension), Sigma = diag(dimension)))
    oc_indices <- c(201:300, 501:600, 801:900)
    ic_indices <- c(1:200, 301:500, 601:800, 901:1000)
  } else if(ph2_scenario == 4) {
    dat <- mvrnorm(1000, mu = rep(0, dimension), Sigma = diag(dimension))
    shifted_obs <- sample(1:1000, 500)
    dat[shifted_obs, ] <- dat[shifted_obs, ] + shift
    oc_indices <- shifted_obs
    ic_indices <- (1:1000)[-shifted_obs]
  }
  
  df <- rbind(dat_train, dat)
  
  tb <- tibble('Index' = 1:nrow(df), 'Value' = df[, 1])
  tb$clean <- tb$Value
  tb$clean[contamination_indices] <- tb$clean[contamination_indices] - shift
  # Function to add fault information/lines to the plot based on the Phase II scenario
  if(ph2_scenario == 1) {
    # tibble('Index' = 1:nrow(df), 'Variable 1' = df[, 1], 'Variable 2' = df[, 2], 'Variable 3' = df[, 3]) |> 
    #   pivot_longer(2:4, names_to = 'Variable', values_to = 'Value') |> 
    # ggplot(aes(Index, Value, col = Variable)) +
    ggplot() +
      annotate('rect', xmin = 0, xmax = m0, ymin = -10, ymax = 10, fill = 'gray', alpha = 0.5, color = NA) +
      annotate('rect', xmin = m0, xmax = nrow(df), ymin = -10, ymax = 10, fill = 'firebrick1', alpha = 0.5, color = NA) +
      annotate('text', x = m0/2, y = -4.4, label = 'Phase I', color = 'black', size = 5) +
      annotate('text', x = m0 + ((1 - (m0 / nrow(df)))/2)*nrow(df), y = -4.4, label = 'Fault', color = 'black', size = 5) +
      annotate('text', x = m0/2 + 20, y = 5, label = 'Contamination', color = 'black', size = 3) +
      annotate('point', x = 30, y = 5, color = 'steelblue') +
      geom_line(aes(tb$Index, tb$Value)) +
      geom_point(aes(tb$Index[contamination_indices], tb$Value[contamination_indices]), color = 'steelblue', size = 0.75) +
      ggtitle(bquote('Realization of Scenario '*.(ph2_scenario))) +
      coord_cartesian(ylim = c(-5, 5)) +
      theme_light() +
      theme(legend.position="none") +
      scale_x_continuous(breaks = seq(0, m0 + 1000, by = 100))
  } else if(ph2_scenario == 2) {
    ggplot() +
      annotate('rect', xmin = 0, xmax = m0, ymin = -10, ymax = 10, fill = 'gray', alpha = 0.5, color = NA) +
      annotate('rect',xmin = m0 + 200, xmax = nrow(df), ymin = -10, ymax = 10, fill = 'firebrick1', alpha = 0.5, color = NA) +
      annotate('text', x = m0/2, y = -4.4, label = 'Phase I', color = 'black', size = 5) +
      annotate('text', x = m0 + 200 + ((1 - ((m0 + 200) / nrow(df)))/2)*nrow(df), y = -4.4, label = 'Fault', color = 'black', size = 5) +
      annotate('text', x = m0/2 + 20, y = 5, label = 'Contamination', color = 'black', size = 3) +
      annotate('point', x = 30, y = 5, color = 'steelblue') +
      geom_line(aes(tb$Index, tb$Value)) +
      geom_point(aes(tb$Index[contamination_indices], tb$Value[contamination_indices]), color = 'steelblue', size = 0.75) +
      ggtitle(bquote('Realization of Scenario '*.(ph2_scenario))) +
      coord_cartesian(ylim = c(-5, 5)) +
      theme_light() +
      theme(legend.position="none") +
      scale_x_continuous(breaks = seq(0, m0 + 1000, by = 100))
  } else if(ph2_scenario == 3) {
    ggplot() +
      annotate('rect', xmin = 0, xmax = m0, ymin = -10, ymax = 10, fill = 'gray', alpha = 0.5, color = NA) +
      annotate('rect', xmin = m0 + 200, xmax = m0 + 300, ymin = -10, ymax = 10, fill = 'firebrick1', alpha = 0.5, color = NA) +
      annotate('rect', xmin = m0 + 500, xmax = m0 + 600, ymin = -10, ymax = 10, fill = 'firebrick1', alpha = 0.5, color = NA) +
      annotate('rect', xmin = m0 + 800, xmax = m0 + 900, ymin = -10, ymax = 10, fill = 'firebrick1', alpha = 0.5, color = NA) +
      annotate('text', x = m0/2, y = -4.4, label = 'Phase I', color = 'black', size = 5) +
      annotate('text', x = m0 + 250, y = -4.4, label = 'Fault', color = 'black', size = 5) +
      annotate('text', x = m0 + 550, y = -4.4, label = 'Fault', color = 'black', size = 5) +
      annotate('text', x = m0 + 850, y = -4.4, label = 'Fault', color = 'black', size = 5) +
      annotate('text', x = m0/2 + 20, y = 5, label = 'Contamination', color = 'black', size = 3) +
      annotate('point', x = 30, y = 5, color = 'steelblue') +
      geom_line(aes(tb$Index, tb$Value)) +
      geom_point(aes(tb$Index[contamination_indices], tb$Value[contamination_indices]), color = 'steelblue', size = 0.75) +
      ggtitle(bquote('Realization of Scenario '*.(ph2_scenario))) +
      coord_cartesian(ylim = c(-5, 5)) +
      theme_light() +
      theme(legend.position="none") +
      scale_x_continuous(breaks = seq(0, m0 + 1000, by = 100))
  } else if(ph2_scenario == 4) {
    ggplot() +
      annotate('rect', xmin = 0, xmax = m0, ymin = -10, ymax = 10, fill = 'gray', alpha = 0.5, color = NA) +
      geom_vline(xintercept = m0 + oc_indices, col = 'firebrick1', alpha = 0.2) +
      annotate('text', x = m0/2, y = -4.4, label = 'Phase I', color = 'black', size = 5) +
      annotate('text', x = m0 + ((1 - (m0 / nrow(df)))/2)*nrow(df), y = -4.4, label = 'Faults', color = 'black', size = 5) +
      annotate('text', x = m0/2 + 20, y = 5, label = 'Contamination', color = 'black', size = 3) +
      annotate('point', x = 30, y = 5, color = 'steelblue') +
      geom_line(aes(tb$Index, tb$Value)) +
      geom_point(aes(tb$Index[contamination_indices], tb$Value[contamination_indices]), color = 'steelblue', size = 0.75) +
      ggtitle(bquote('Realization of Scenario '*.(ph2_scenario))) +
      # ggtitle(bquote("Realization of Scenario "*.(ph2_scenario)*'; '*italic(p)*' = 3; '*pi*' = '*.(contamination))) +
      coord_cartesian(ylim = c(-5, 5)) +
      theme_light() +
      theme(legend.position="none") +
      scale_x_continuous(breaks = seq(0, m0 + 1000, by = 100))
  }
}

realization_plot(m0 = 250, ph2_scenario = 1, contamination = 0.2, shift = 2.25)
ggsave('scenario1_realization_points.pdf', height = 7/4, width = 7)

realization_plot(m0 = 250, ph2_scenario = 2, contamination = 0.2, shift = 2.25)
ggsave('scenario2_realization_points.pdf', height = 7/4, width = 7)

realization_plot(m0 = 250, ph2_scenario = 3, contamination = 0.2, shift = 2.25)
ggsave('scenario3_realization_points.pdf', height = 7/4, width = 7)

realization_plot(m0 = 250, ph2_scenario = 4, contamination = 0.2, shift = 2.25)
ggsave('scenario4_realization_points.pdf', height = 7/4, width = 7)

