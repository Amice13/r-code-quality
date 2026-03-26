estimate_models <- function(data, variables) {
  models <- list()
  
  for (var in variables) {
    filtered_data <- data %>% 
      filter(prompt == var)
    
    model <- lm(formula = score ~ treatment + treatment * ideo_group, data = filtered_data)
    
    models[[var]] <- model
  }
  
  return(models)
}

reg_results_nofe <- function(data, filter = "All"){
  reg <- lm_robust(
    dv ~ treatment,
    fixed_effects =  ~ ideo_group,
    se_type = 'HC2',
    data,
    weights = weight
  )
  
  return(
    tidy(reg) %>%
      mutate(regression = filter)
  )
}

reg_results <- function(data, filter = "All", att_lev = "High", fe = NULL){
  
  if(filter != "All"){
    #Only when FE are not needed
    if(is.null(fe)){
      reg <- lm_robust(
        dv ~ treatment,
        se_type = 'HC2',
        data  %>%
          filter(ideo_group == filter)
      )
      
    }else{
      
      reg <- lm_robust(
        dv ~ treatment,
        fixed_effects = ~ prompt,
        se_type = 'HC2',
        data %>%
          filter(ideo_group == filter) %>% 
          filter(attention_level == att_lev)
      )
    }
    
  } else {
    
    reg <- lm_robust(
      dv ~ treatment,
      fixed_effects =  ~ ideo_group + prompt,
      se_type = 'HC2',
      data,
      weights = weight
    )
  }
  
  return(
    tidy(reg) %>%
      mutate(regression = att_lev))
}

plot_reg_results <- function(data, title ="My title", x = "My x", y = "My y", caption = "My caption"){
  data %>%
    ggplot(aes(x = estimate,
               y = term, 
               xmin = conf.low, 
               xmax = conf.high, 
               color = regression, 
               shape = regression
    )
    ) +
    geom_pointrange(
      position = position_dodge(width = 0.5)
    ) +
    labs(
      title = title,
      x = x,
      y = y,
      caption = caption,
      shape = "Number of attention checks failed") +
    scale_y_discrete(
      labels = c(
        "treatmentCrude" = "Crude Style",
        "treatmentJournalistic" = "Journalistic Style"
      )
    ) +
    guides(color = guide_legend(title = "Number of attention checks failed")) +
    theme_bw() +
    geom_vline(
      xintercept = 0,
      linetype="dotted",
      color = "red",
      linewidth=0.5
    )
  
}
