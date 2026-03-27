#' Estimate and Plot Covariate Balance Tests
#'
#' @param shock_in Character. Shock variable name
#' @param wt_in Character. Weight variable name
#' @param fig_label Character. Figure label
#' @return Invisible. Creates balance plot
est_and_plot_equiv <- function(shock_in, wt_in, fig_label) {
  
  if (!wt_in %in% names(g)) {
    stop("Weight variable '", wt_in, "' not found in data. Cannot proceed with equivalence test.")
  }
    
  equiv_out <- purrr::map(varlist, ~ est_equiv(.x, treat = shock_in, w = wt_in))
  
  equiv_out <- do.call("rbind", equiv_out)
  rope_low <- equiv_out$ROPE_low[[1]]
  rope_high <- equiv_out$ROPE_high[[1]]
  rownames(equiv_out) <- NULL
  equiv_out <- subset(equiv_out, !grepl("globalreg", Parameter))
  parm_names <- subset(equiv_out, sample == "Adjusted", select = Parameter)
  p_bal <- equiv_out %>%
    dplyr::mutate(Parameter = factor(Parameter, ordered = TRUE, levels = parm_names$Parameter)) %>%
    ggplot() +
    geom_hline(yintercept = 0, color = "grey") +
    geom_hline(yintercept = rope_low, lty = "dashed") +
    geom_hline(yintercept = rope_high, lty = "dashed") +
    geom_pointrange(
      aes(x = Parameter, y = estimate, ymin = CI_low, ymax = CI_high, color = sample, shape = sample),
      linewidth = lwidth, size = ptsize * .8
    ) +
    coord_flip() +
    theme_bw(base_size = 14) +
    scale_color_manual(values = c("blue", "orange")) +
    labs(y = "Covariate-Treatment x Moderator Correlation", x = "", 
         color = "", shape = "") +
    scale_x_discrete(labels = varmap) +
    scale_y_continuous(limits = c(-1, 1)) +
    theme(
      panel.grid = element_blank(),
      legend.position = c(.85, .95),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
  ggsave(
    p_bal,
    filename = here("Output", "figures", paste0("fig_", fig_label, "_balance_wrp_", shock_in, ".pdf")), 
    width = fwidth,
    height = 6,
    scale = fscale
  )
}
