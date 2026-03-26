library(ggExtra)

#Write latex tables with notes:
write_latex = function(star_table, note_text, out_path, scale) {
  
  table_note = note_text %>% 
    str_replace_all("\n", " ")
  
  preamble = c( "\\scalebox{", scale, "}{ 
                \\begin{threeparttable}")
  
  out_table = append(star_table, 
                     preamble,
                     str_detect(star_table, "centering") %>% which) %>%
    append(.,
           c("\\begin{tablenotes}", "\\small", 
             paste0("\\item \\textit{Note}: $^\\dag$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01.", table_note), 
             "\\end{tablenotes}", "\\end{threeparttable}}"),
           str_detect(., "[\\\\]end\\{tabular\\}") %>% which)
  
  cat(out_table, sep = '\n', file = out_path)
}


# Define the function to write HTML output with the note at the bottom
write_html  = function(star_table, note_text, out_path) {
  table_note <- str_replace_all(note_text, "\n", " ")
  note_html <- paste0("<p><em>Note:</em> <sup>&dagger;</sup>p&lt;0.1; ",
                      "<sup>*</sup>p&lt;0.05; <sup>**</sup>p&lt;0.01. ",
                      table_note, "</p>")
  out_table <- c(star_table, note_html)
  cat(out_table, sep = "\n", file = out_path)
}


#Write latex tables with notes:
write_latex_plain = function(star_table, note_text, out_path) {
  
  cat(star_table, sep = '\n', file = out_path)
}


#Write latex tables with notes:
write_latex_placebo = function(star_table, note_text, out_path, scale) {
  
  table_note = note_text %>% 
    str_replace_all("\n", " ")
  
  preamble = c( "\\scalebox{", scale, "}{ 
                \\begin{threeparttable}")
  
  out_table = append(star_table, 
                     preamble,
                     str_detect(star_table, "centering") %>% which) %>%
    append(.,
           c("\\begin{tablenotes}", "\\small", 
             paste0("\\item \\textit{Note}: ", table_note), 
             "\\end{tablenotes}", "\\end{threeparttable}}"),
           str_detect(., "[\\\\]end\\{tabular\\}") %>% which)
  
  cat(out_table, sep = '\n', file = out_path)
}


loess_plot <- function(x, y, W, ylab){
  
  ##making the loess fit
  tmp <- na.omit(data.frame(x = x, y = y, W = W))
  #tmp <- tmp[tmp$x > -0.30 & tmp$x < 0.30,]
  loess_reg <- loess(y ~ x * W, data = tmp, degree = 1)
  tmp$y_fitted <- fitted.values(loess_reg)
  tmp$se <- predict(loess_reg, tmp, se = TRUE)$se.fit
  
  
  #making plots
  p <- ggplot(data = tmp, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = y_fitted - 1.67 * se, ymax = y_fitted + 1.67 * se, group = W), alpha = .2, linetype = "dashed", color="darkgrey") +
    geom_line(aes(y = y_fitted, group = W), color = 'black') + 
    theme_bw() + 
    xlab("Distance to border (meters)") + 
    ylab(ylab) + 
    scale_x_continuous(breaks = seq(-15000, 15000, 5000)) +
    theme(text=element_text(size=10, family="Times")) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank()) + 
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    stat_summary_bin(fun.y='mean', bins=18,
                     color='black', size=2, geom='point', alpha = 0.4) 
  
  return(p)
}

loess_plot_weighted <- function(x, y, W, ylab, weights, bin_position, bin_val){
  
  
  ##making the loess fit
  tmp <- na.omit(data.frame(x = x, y = y, W = W, weights = weights))
  #tmp <- tmp[tmp$x > -0.30 & tmp$x < 0.30,]
  loess_reg <- loess(y ~ x * W, weights = weights, data = tmp, degree = 1)
  tmp$y_fitted <- fitted.values(loess_reg)
  tmp$se <- predict(loess_reg, tmp, se = TRUE)$se.fit
  
  binned =
    data.frame(bin_position = bin_position,
               bin_val = bin_val) %>%
    distinct()
  
  
  #making plots
  p <- ggplot(data = tmp, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = y_fitted - 1.67 * se, ymax = y_fitted + 1.67 * se, group = W), alpha = .2, linetype = "dashed", color="darkgrey") +
    geom_line(aes(y = y_fitted, group = W), color = 'black') + 
    theme_bw() + 
    xlab("Distance to border (meters)") + 
    ylab(ylab) + 
    scale_x_continuous(breaks = seq(-15000, 15000, 5000)) +
    theme(text=element_text(size=10, family="Times")) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank()) + 
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    geom_point(data = binned, aes(x=bin_position, y = bin_val), color = "black", size = 2, alpha = 0.4)
  
  return(p)
}




mod_plot <- function(x, y, d, ylab, xlab, title){
  
  
  tmp <- na.omit(data.frame(x = x, y = y, d = d))
  
  b0 <- summary(lm(y ~ x, data = tmp[tmp$d == 0,]))[[4]][2,1]
  se0 <- summary(lm(y ~ x, data = tmp[tmp$d == 0,]))[[4]][2,2]
  
  b1 <- summary(lm(y ~ x, data = tmp[tmp$d == 1,]))[[4]][2,1]
  se1 <- summary(lm(y ~ x, data = tmp[tmp$d == 1,]))[[4]][2,2]
  
  tmp = cbind(c(b0, b1), c(se0, se1), c("No", "Yes")) %>% 
    data.frame()
  
  tmp$X1 = as.numeric(as.character(tmp$X1))
  tmp$X2 = as.numeric(as.character(tmp$X2))
  
  
  ggplot(tmp, aes(x=factor(X3), y = as.numeric(X1), group = 1)) + 
    geom_point(color = "black", fill = "white", size = 2) + 
    geom_errorbar(aes(ymin=X1 - 1.96*as.numeric(X2), ymax = X1 + 1.96*as.numeric(X2)),
                  width = 0.075, color = "red", position=position_dodge(width=0.5)) +
    theme_minimal() + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    geom_line(alpha = 0.5) + 
    xlab(xlab) + 
    ylab(ylab) +
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text=element_text(size=12, family="Times")) %>%
    return()
  
  
}



#regression functions ------------------------------------------------
first_stage = function(df, bw){
  lm_object = 
    df %>%
    filter(abs(forcing_f1) < bw) %>%
    lm(no_job ~ f1, data = ., na.action = na.omit)
  
  lm_object$na.action = NA
  return(lm_object)
}


second_stage = function(df, bw, var){
  var = enquo(var)
  df %>%
    filter(abs(forcing_f1) < bw) %>%
    mutate(outcome = !!var) %>%
    mutate(fitted = lm(no_job~ f1, data = .)$fitted) %>%
    lm_robust(outcome ~ fitted, data = .)
}

second_stage_tab = function(df, bw, var){
  var = enquo(var)
  df %>%
    filter(abs(forcing_f1) < bw) %>%
    mutate(outcome = !!var) %>%
    mutate(fitted = lm(no_job~ f1, data = .)$fitted) %>%
    lm(outcome ~ fitted, data = .)
}


second_stage_tab_ll = function(df, bw, var){
  var = enquo(var)
  df %>%
    filter(abs(forcing_f1) < bw) %>%
    mutate(outcome = !!var) %>%
    mutate(fitted = lm(no_job~ f1*forcing_f1, data = .)$fitted) %>%
    lm(outcome ~ fitted*forcing_f1, data = .)
}


rdd_plot_binned_t1 = function(df, var, bw, label){
  
  var = enquo(var)
  
  df %<>% 
    filter(!is.infinite(forcing_final), !is.na(forcing_final)) %>%
    filter(abs(forcing_final) < bw) %>%
    mutate(treat = (forcing_final > 0)*1) %>%
    mutate(bins = cut(forcing_final, breaks = seq(from = (0-bw), to = (0+bw), by=1), dig.lab = 6)) %>%
    mutate(outcome = !!var) %>%
    group_by(bins) %>%
    mutate(n_obs = n()) %>%
    mutate(outcome_binned = mean(outcome, na.rm = T)) %>%
    mutate(bins_split = bins,
           bins_split = str_replace_all(bins_split, "\\(|\\)|\\[|\\]", "")) %>%
    separate(bins_split, into = c("start_bin", "end_bin"), sep = ",") %>%
    mutate_at(vars(start_bin, end_bin), funs(as.numeric(.))) %>%
    mutate(bin_position = (start_bin + end_bin)/2)
  
  bin_df = 
    df %>% 
    dplyr::select(bin_position, outcome_binned, n_obs) %>% 
    distinct() %>% 
    mutate(treat = NA_real_)
  
  
  ylim_min = min(bin_df$outcome_binned)
  ylim_max = max(bin_df$outcome_binned)
  diff = abs(ylim_max - ylim_min)
  ylim_min = ylim_min - diff
  ylim_max = ylim_max + diff
  
  plot =
    df %>%
    ggplot(aes(x = forcing_final, y = outcome, group = treat)) + 
    geom_vline(aes(xintercept = 0), color = 'black', size = 0.5, linetype = 'dashed') + 
    geom_smooth(method = "lm", color = "black",size = 0.4, fill = "transparent") +
    theme_bw() +
    geom_point(data = bin_df, aes(x=bin_position, y = outcome_binned, size = n_obs), color = "black", fill = alpha("#bcbddc", 0.4), shape = 21, stroke = 0.75) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major.x = element_blank(),
          axis.line.y.left = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank()) +
    xlab("Integrated Score Difference") +
    ylab(label) +
    coord_cartesian(ylim = c(ylim_min, ylim_max)) +
    theme(legend.position = "none") +
    #theme(text=element_text(size=12, family="lmroman")) +
    annotate("text",
             x = 2,
             y = ylim_min,
             label = "got job",
             #family = "lmroman",
             color = "black") +
    annotate("text",
             x = -2,
             y = ylim_min,
             label = "no job",
             #family = "lmroman",
             color = "black") +
    geom_segment(aes(x = -3.75, y = ylim_min, xend = -9, yend = ylim_min), colour='black', size=0.3,arrow = arrow(length = unit(0.3, "cm"))) +
    geom_segment(aes(x = 3.75, y = ylim_min, xend = 9, yend = ylim_min), colour='black', size=0.3,arrow = arrow(length = unit(0.3, "cm")))
  
  return(plot)
  
}


rdd_plot_binned_f3 = function(df, var, bw, label){
  
  var = enquo(var)
  
  df %<>% 
    filter(!is.infinite(forcing_skd), !is.na(forcing_skd)) %>%
    filter(abs(forcing_skd) < bw) %>%
    mutate(treat = (forcing_skd > 0)*1) %>%
    mutate(bins = cut(forcing_skd, breaks = seq(from = (0-bw), to = (0+bw), by=5), dig.lab = 6)) %>%
    mutate(outcome = !!var) %>%
    group_by(bins) %>%
    mutate(n_obs = n()) %>%
    mutate(outcome_binned = mean(outcome, na.rm = T)) %>%
    mutate(bins_split = bins,
           bins_split = str_replace_all(bins_split, "\\(|\\)|\\[|\\]", "")) %>%
    separate(bins_split, into = c("start_bin", "end_bin"), sep = ",") %>%
    mutate_at(vars(start_bin, end_bin), funs(as.numeric(.))) %>%
    mutate(bin_position = (start_bin + end_bin)/2)
  
  bin_df = 
    df %>% 
    dplyr::select(bin_position, outcome_binned, n_obs) %>% 
    distinct() %>% 
    mutate(treat = NA_real_)
  
  ylim_min = min(bin_df$outcome_binned)
  ylim_max = max(bin_df$outcome_binned)
  diff = abs(ylim_max - ylim_min)
  ylim_min = ylim_min - diff
  ylim_max = ylim_max + diff
  
  plot =
    df %>%
    ggplot(aes(x = forcing_skd, y = outcome, group = treat)) + 
    geom_vline(aes(xintercept = 0), color = 'black', size = 0.5, linetype = 'dashed') + 
    geom_smooth(method = "lm", color = "black",size = 0.4, fill = "transparent") +
    theme_bw() +
    geom_point(data = bin_df, aes(x=bin_position, y = outcome_binned, size = n_obs), color = "black", fill = alpha("#bcbddc", 0.4), shape = 21, stroke = 0.75) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major.x = element_blank(),
          axis.line.y.left = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank()) +
    xlab("Distance to SKD Threshold") +
    ylab(label) +
    coord_cartesian(ylim = c(ylim_min, ylim_max)) +
    theme(legend.position = "none") +
    #theme(text=element_text(size=12, family="lmroman")) +
    annotate("text",
             x = 10,
             y = ylim_min,
             label = "passed",
             #family = "lmroman",
             color = "black") +
    annotate("text",
             x = -10,
             y = ylim_min,
             label = "failed",
             #family = "lmroman",
             color = "black") +
    geom_segment(aes(x = -18, y = ylim_min, xend = -45, yend = ylim_min), colour='black', size=0.3,arrow = arrow(length = unit(0.3, "cm"))) +
    geom_segment(aes(x = 18, y = ylim_min, xend = 45, yend = ylim_min), colour='black', size=0.3,arrow = arrow(length = unit(0.3, "cm")))
  
  return(plot)
  
}




kling_index = function(.data, .impute, ...){
  
  index_vars = enquos(...)
  for(i in index_vars){
    
    control_avg = .data %>% filter(pass_final == 0) %>% summarise(mean(!!i, na.rm=T)) %>% as.numeric()
    control_sd = .data %>% filter(pass_final == 0) %>% summarise(sd(!!i, na.rm=T)) %>% as.numeric()
    
    .data =
      .data %>% 
      mutate(zscore =
               (!!i - control_avg)/control_sd)
    
    .data$zscore = as.numeric(.data$zscore)
    
    if (.impute == T){
      grp_mean_ctrl = .data %>% filter (pass_final == 0) %>% summarise(mean(zscore,na.rm=T)) %>% as.numeric()
      grp_mean_trt = .data %>% filter (pass_final == 1) %>% summarise(mean(zscore,na.rm=T)) %>% as.numeric()
      
      
      .data = 
        .data %>%
        mutate(zscore = case_when(is.na(zscore) & pass_final == 0 ~ grp_mean_ctrl,
                                  is.na(zscore) & pass_final == 1 ~ grp_mean_trt,
                                  TRUE ~ zscore))
      
      
    }
    
    names(.data)[names(.data) == "zscore"] = paste0("zscore_", quo_name(i))
    
    
  }
  .data$zscore = rowMeans(.data[,grep("^zscore_", names(.data))], na.rm = T) / sd(rowMeans(.data[.data$pass_final == 0,grep("^zscore_", names(.data))], na.rm = T), na.rm = T)
  return(.data$zscore)
  
}


kling_index_t1 = function(.data, .impute, ...){
  
  index_vars = enquos(...)
  for(i in index_vars){
    
    control_avg = .data %>% filter(pass_final == 0) %>% summarise(mean(!!i, na.rm=T)) %>% as.numeric()
    control_sd = .data %>% filter(pass_final == 0) %>% summarise(sd(!!i, na.rm=T)) %>% as.numeric()
    
    .data =
      .data %>% 
      mutate(zscore =
               (!!i - control_avg)/control_sd)
    
    .data$zscore = as.numeric(.data$zscore)
    
    if (.impute == T){
      grp_mean_ctrl = .data %>% filter (pass_final == 0) %>% summarise(mean(zscore,na.rm=T)) %>% as.numeric()
      grp_mean_trt = .data %>% filter (pass_final == 1) %>% summarise(mean(zscore,na.rm=T)) %>% as.numeric()
      
      
      .data = 
        .data %>%
        mutate(zscore = case_when(is.na(zscore) & pass_final == 0 ~ grp_mean_ctrl,
                                  is.na(zscore) & pass_final == 1 ~ grp_mean_trt,
                                  TRUE ~ zscore))
      
      
    }
    
    names(.data)[names(.data) == "zscore"] = paste0("zscore_", quo_name(i))
    
    
  }
  .data$zscore = rowMeans(.data[,grep("^zscore_", names(.data))], na.rm = T) / sd(rowMeans(.data[.data$pass_final == 0,grep("^zscore_", names(.data))], na.rm = T), na.rm = T)
  return(.data$zscore)
  
}

kling_index_matriculant = function(.data, .impute, ...){
  
  index_vars = enquos(...)
  for(i in index_vars){
    
    control_avg = .data %>% filter(matriculant == 0) %>% summarise(mean(!!i, na.rm=T)) %>% as.numeric()
    control_sd = .data %>% filter(matriculant == 0) %>% summarise(sd(!!i, na.rm=T)) %>% as.numeric()
    
    .data =
      .data %>% 
      mutate(zscore =
               (!!i - control_avg)/control_sd)
    
    .data$zscore = as.numeric(.data$zscore)
    
    if (.impute == T){
      grp_mean_ctrl = .data %>% filter (matriculant == 0) %>% summarise(mean(zscore,na.rm=T)) %>% as.numeric()
      grp_mean_trt = .data %>% filter (matriculant == 1) %>% summarise(mean(zscore,na.rm=T)) %>% as.numeric()
      
      
      .data = 
        .data %>%
        mutate(zscore = case_when(is.na(zscore) & matriculant == 0 ~ grp_mean_ctrl,
                                  is.na(zscore) & matriculant == 1 ~ grp_mean_trt,
                                  TRUE ~ zscore))
      
      
    }
    
    names(.data)[names(.data) == "zscore"] = paste0("zscore_", quo_name(i))
    
    
  }
  .data$zscore = rowMeans(.data[,grep("^zscore_", names(.data))], na.rm = T) / sd(rowMeans(.data[.data$matriculant == 0,grep("^zscore_", names(.data))], na.rm = T), na.rm = T)
  return(.data$zscore)
  
}


kling_index_f3 = function(.data, .impute, ...){
  
  
  index_vars = enquos(...)
  
  #.data = .data %>% dplyr::select_(!!index_vars, fail_skd)
  
  for(i in index_vars){
    
    control_avg = .data %>% filter(fail_skd == 0) %>% summarise(mean(!!i, na.rm=T)) %>% as.numeric()
    control_sd = .data %>% filter(fail_skd == 0) %>% summarise(sd(!!i, na.rm=T)) %>% as.numeric()
    
    .data =
      .data %>% 
      mutate(zscore =
               (!!i - control_avg)/control_sd)
    
    .data$zscore = as.numeric(.data$zscore)
    
    
    if (.impute == T){
      grp_mean_ctrl = .data %>% filter (fail_skd == 0) %>% summarise(mean(zscore,na.rm=T)) %>% as.numeric()
      grp_mean_trt = .data %>% filter (fail_skd == 1) %>% summarise(mean(zscore,na.rm=T)) %>% as.numeric()
      
      
      .data = 
        .data %>%
        mutate(zscore = case_when(is.na(zscore) & fail_skd == 0 ~ grp_mean_ctrl,
                                  is.na(zscore) & fail_skd == 1 ~ grp_mean_trt,
                                  TRUE ~ zscore))
    }
    
    names(.data)[names(.data) == "zscore"] = paste0("zscore_", quo_name(i))
    
    
  }
  .data$zscore = rowMeans(.data[,grep("^zscore_", names(.data))], na.rm = T) / sd(rowMeans(.data[.data$fail_skd == 0,grep("^zscore_", names(.data))], na.rm = T), na.rm = T)
  return(.data$zscore)
  
}




rdd_plot_binned_bkn = function(df, var, bw, label){
  
  var = enquo(var)
  
  
  df %<>% 
    filter(!is.infinite(forcing_t1), !is.na(forcing_t1)) %>%
    filter(abs(forcing_t1) < bw) %>%
    mutate(treat = (forcing_t1 > 0)*1) %>%
    mutate(bins = cut(forcing_t1, breaks = seq(from = (0-bw), to = (0+bw), by=2), dig.lab = 6)) %>%
    mutate(outcome = !!var) %>%
    group_by(bins) %>%
    mutate(n_obs = n()) %>%
    mutate(outcome_binned = mean(outcome, na.rm = T)) %>%
    mutate(bins_split = bins,
           bins_split = str_replace_all(bins_split, "\\(|\\)|\\[|\\]", "")) %>%
    separate(bins_split, into = c("start_bin", "end_bin"), sep = ",") %>%
    mutate_at(vars(start_bin, end_bin), funs(as.numeric(.))) %>%
    mutate(bin_position = (start_bin + end_bin)/2)
  
  bin_df = 
    df %>% 
    select(bin_position, outcome_binned, n_obs, forcing_t1) %>% 
    distinct() %>% 
    mutate(treat = (forcing_t1 > 0)*1)
  
  
  plot =
    bin_df %>%
    ggplot(aes(x = bin_position, y = outcome_binned, group = factor(treat))) + 
    geom_vline(aes(xintercept = 0), color = 'black', size = 0.5) + 
    scale_color_brewer(NULL, type = 'qual', palette = 6) +
    #geom_smooth(method = "lm", color = "darkgrey",size = 0.75, fill = "transparent", linetype = "dashed") +
    geom_line(size = 0.5, color = "black") +
    theme_bw() +
    geom_point(aes(size = n_obs), color = "#756bb1", shape = 21, stroke = 1.25) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major.x = element_blank(),
          axis.line.y.left = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
    xlab("Integrated Score Difference") +
    ylab(label) +
    coord_cartesian(ylim = c(2, 4)) +
    scale_y_continuous(breaks = 1:4,
                       labels = c("Not at all \n transparent", "Not very \n transparent" ,"Somewhat \n transparent", "Very \n transparent")) +
    theme(legend.position = "none") +
    theme(text=element_text(size=14, family="monserrat")) +
    annotate("text",
             x = 2,
             y = 2,
             label = "got job",
             family = "monserrat",
             color = "black") +
    annotate("text",
             x = -2,
             y = 2,
             label = "no job",
             family = "monserrat",
             color = "black") +
    geom_segment(aes(x = -3.75, y = 2, xend = -9, yend = 2), colour='black', size=0.3,arrow = arrow(length = unit(0.3, "cm"))) +
    geom_segment(aes(x = 3.75, y = 2, xend = 9, yend = 2), colour='black', size=0.3,arrow = arrow(length = unit(0.3, "cm")))
  
  
  return(plot)
  
}

rdd_plot_binned_bkn_income = function(df, var, bw, label){
  
  var = enquo(var)
  
  
  df %<>% 
    filter(!is.infinite(forcing_t1), !is.na(forcing_t1)) %>%
    filter(abs(forcing_t1) < bw) %>%
    mutate(treat = (forcing_t1 > 0)*1) %>%
    mutate(bins = cut(forcing_t1, breaks = seq(from = (0-bw), to = (0+bw), by=1), dig.lab = 6)) %>%
    mutate(outcome = !!var) %>%
    group_by(bins) %>%
    mutate(n_obs = n()) %>%
    mutate(outcome_binned = mean(outcome, na.rm = T)) %>%
    mutate(bins_split = bins,
           bins_split = str_replace_all(bins_split, "\\(|\\)|\\[|\\]", "")) %>%
    separate(bins_split, into = c("start_bin", "end_bin"), sep = ",") %>%
    mutate_at(vars(start_bin, end_bin), funs(as.numeric(.))) %>%
    mutate(bin_position = (start_bin + end_bin)/2)
  
  bin_df = 
    df %>% 
    select(bin_position, outcome_binned, n_obs, forcing_t1) %>% 
    distinct() %>% 
    mutate(treat = (forcing_t1 > 0)*1)
  
  
  plot =
    bin_df %>%
    ggplot(aes(x = bin_position, y = outcome_binned, group = factor(treat))) + 
    geom_vline(aes(xintercept = 0), color = 'black', size = 0.5) + 
    scale_color_brewer(NULL, type = 'qual', palette = 6) +
    #geom_smooth(method = "lm", color = "darkgrey",size = 0.75, fill = "transparent", linetype = "dashed") +
    geom_line(size = 0.5, color = "black") +
    theme_bw() +
    geom_point(aes(size = n_obs), color = "#756bb1", shape = 21, stroke = 1.25) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major.x = element_blank(),
          axis.line.y.left = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
    xlab("Integrated Score Difference") +
    ylab(label) +
    coord_cartesian(ylim = c(2, 5)) +
    scale_y_continuous(breaks = 1:8,
                       labels = c("<1m IDR", "1-2m IDR", "2-3m IDR", "3-4m IDR", "4-5m IDR", "5-6m IDR", "6-7m IDR", ">7m IDR")) +
    theme(legend.position = "none") +
    theme(text=element_text(size=14, family="monserrat")) +
    annotate("text",
             x = 2,
             y = 2,
             label = "got job",
             family = "monserrat",
             color = "black") +
    annotate("text",
             x = -2,
             y = 2,
             label = "no job",
             family = "monserrat",
             color = "black") +
    geom_segment(aes(x = -3.75, y = 2, xend = -9, yend = 2), colour='black', size=0.3,arrow = arrow(length = unit(0.3, "cm"))) +
    geom_segment(aes(x = 3.75, y = 2, xend = 9, yend = 2), colour='black', size=0.3,arrow = arrow(length = unit(0.3, "cm")))
  
  
  return(plot)
  
}
