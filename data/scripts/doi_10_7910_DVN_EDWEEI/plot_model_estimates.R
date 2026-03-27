#' Create plot of model estimates including meta analysis
#' 
#' @param model.obj List. Output from est_models function
#' @param xlb Numeric. Lower bound for x-axis
#' @param xub Numeric. Upper bound for x-axis
#' @param N Numeric. Number of respondents
#' @param zones Numeric. Number of zones
#' @param countries Numeric. Number of countries
#' @param xlab Character. Label for x-axis
#' @param fire Logical. Whether to use labels for fire robustness checks
#' 
#' @return ggplot object
#' 
#' @details
#' Function extracts results from est_models function and creates a plot of the estimates.
#' Includes meta-analysis of the estimates.
#' 
#' @examples
#' \dontrun{
#' # Requires global variables: hetvars, covars
#' results <- plot_model_estimates(model.obj)
#' }
#' 
#' @note Relies on global variables for plot styling: lwidth, ptsize
plot_model_estimates <- function(
    model.obj,
    xlb = -.015,
    xub = .015,
    N = 148712,
    zones = 2458,
    countries = 137,
    xlab = "Average effect of +1SD climate shock on climate risk perceptions",
    fire = FALSE) {
  
  model_out <- extract_results(nested_list = model.obj)
  
  tab_out <- model_out$first
  
  htest <- model_out$second %>%
    dplyr::mutate(
      sig = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1 ~ "*",
        TRUE ~ ""
      )
    ) %>%
    dplyr::select(term, model, sig) %>%
    dplyr::mutate(reg_loser_50 = 1)
  
  # Get average effect size
  betas_dam <- subset(tab_out, reg_loser_50 == 1)$estimate
  se_dam <- subset(tab_out, reg_loser_50 == 1)$std.error
  meta_dam <- rma.uni(yi = betas_dam, sei = se_dam)
  
  betas_ben <- subset(tab_out, reg_loser_50 == 0)$estimate
  se_ben <- subset(tab_out, reg_loser_50 == 0)$std.error
  meta_ben <- rma.uni(yi = betas_ben, sei = se_ben)
  
  betas_h <- model_out$second$estimate
  se_h <- model_out$second$std.error
  meta_h <- rma.uni(yi = betas_h, sei = se_h)
  
  meta_df <- data.frame(
    term = "Summary",
    estimate = c(meta_dam$beta, meta_ben$beta, NA),
    conf.low = c(meta_dam$ci.lb, meta_ben$ci.lb, NA),
    conf.high = c(meta_dam$ci.ub, meta_ben$ci.ub, NA),
    reg_loser_50 = c(1, 0, 1),
    model = lab_sum
  )
  
  plot_df <- bind_rows(tab_out, meta_df)
  
  plot_df <- left_join(plot_df, htest, by = c("term", "model", "reg_loser_50"))
  
  plot_df$sig[plot_df$reg_loser_50 == 1 & plot_df$model == "Summary"] <- case_when(
    meta_h$pval < 0.01 ~ "***",
    meta_h$pval < 0.05 ~ "**",
    meta_h$pval < 0.1 ~ "*",
    TRUE ~ ""
  )
  
  plot_df$reg_loser_50 <- ifelse(plot_df$reg_loser_50 == 1, "(b) Severe damage", "(a) Limited damage")
  
  if (isFALSE(fire)) {
    plot_df <- plot_df %>%
      dplyr::mutate(
        term = case_when(
          term == "best_tanom_7d_z" ~ lab_best_7d,
          term == "best_tanom_2sd_7d_z" ~ lab_best_2sd,
          term == "modis_burnanomp_mu_6m_w1_z" ~ lab_fire,
          term == "noaa_cpc_tdev_7d_z" ~ lab_noaa_7d,
          term == "Summary" ~ lab_sum
        )
      )
    
    plot_df$term <- factor(
      plot_df$term,
      levels = c(lab_noaa_7d, lab_best_2sd, lab_best_7d, lab_fire, lab_sum)
    )
  } else {
    plot_df <- plot_df %>%
      dplyr::mutate(
        term = case_when(
          term == "modis_burnanomp_mu_6m_w5_z" ~ lab_fire_1,
          term == "modis_burnanomp_mu_6m_w.5_z" ~ lab_fire_2,
          term == "modis_burnanomp_mu_5m_w1_z" ~ lab_fire_3,
          term == "modis_burnanomp_mu_7m_w1_z" ~ lab_fire_4,
          term == "Summary" ~ lab_sum
        )
      )
    
    plot_df$term <- factor(
      plot_df$term,
      levels = c(lab_fire_1, lab_fire_2, lab_fire_3, lab_fire_4, lab_sum)
    )
  }
    
  
  plot_df$model <- factor(plot_df$model, levels = c("Summary", "Covariates", "CBPS", "CBPS + Covariates"))
  
  pdodge <- .75
  
  p_out <- plot_df %>%
    ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term, color = model, shape = model)) +
    geom_vline(xintercept = 0, color = "grey", linewidth = 1) +
    geom_pointrange(
      data = subset(plot_df, model != "Summary"),
      position = position_dodge(pdodge),
      linewidth = lwidth,
      size = ptsize
    ) +
    geom_meta_diamond_h(
      data = subset(plot_df, model == "Summary"),
      height = .1,
      color = "black",
      fill = "black"
    ) +
    geom_text(aes(label = sig), size = 5, vjust = -.15, position = position_dodge(pdodge), show.legend = FALSE) +
    facet_wrap(~reg_loser_50) +
    scale_y_discrete(labels = label_wrap_gen(30)) +
    scale_x_continuous(limits = c(xlb, xub)) +
    labs(
      x = xlab,
      y = NULL,
      color = "",
      shape = "",
      caption = str_wrap(
       paste(
         "Notes: *p < 0.1, **p < 0.05, ***p < 0.01 indicate significance levels for hypothesis tests comparing the average marginal effect estimates.",
         "Bars denote 95% confidence intervals with robust standard errors clustered by administrative zone.",
         "The outcome indicates whether respondents identify climate change as a top or major risk in their daily lives.",
         "Black diamonds report meta-analysis across the studies with the horizontal bar width corresponding with the same confidence threshold.",
         "CBPS stands for covariate balancing propensity scores.",
         N, "respondents across", zones, "zones in", countries, "countries.",
         sep = " "
       ),
        138
      )
    ) +
    scale_color_manual(values = c(viridis(3), "black"), breaks = c("CBPS", "Covariates", "CBPS + Covariates")) +
    scale_shape_manual(values = c(15, 16, 17, NA), breaks = c("CBPS", "Covariates", "CBPS + Covariates")) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "bottom",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = .5),
      axis.title.y = element_text(angle = 0, margin = margin(0, -90, 0, 0)),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 14, margin = margin(b = 15)),
      axis.text = element_text(color = "black"),
      axis.title.x = element_text(margin = margin(t = 15))
    )
  
  return(p_out)
}
