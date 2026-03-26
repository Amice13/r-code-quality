## Load model results ----
load(file = 'output/hk-replication.Rdata')

# dat = combination of OLS, non-parametric bounds, and Gibbs results
# mod_ = OLS models


## Figure 1 ----
# Estimates of treatment-moderator interaction using pre-test, post-test, and combined data

bind_rows(mod_pre,
          mod_pre_x,
          mod_post,
          mod_post_x,
          mod_prepost,
          mod_prepost_x) %>%
  filter(term == "t:d") %>%
  mutate(
    mod = c("pre", "pre", "post", "post", "prepost", "prepost"),
    covars = rep(c("no", "yes"), 3)
  ) %>%
  mutate(mod = ordered(mod, levels = c("pre", "post", "prepost"))) %>%
  ggplot(
    aes(
      x = mod,
      y = estimate,
      shape = covars,
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    )
  ) +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_hline(lty = "dotted", yintercept = 0) +
  scale_x_discrete(
    labels = c(
      "pre" = "Pre\n(n = 188)",
      "post" = "Post\n(n = 187)",
      "prepost" = "Combined\n(n = 375)"
    )
  ) +
  labs(
    x = "", 
    y = "", 
    shape = "Covariates included?") +
       ylim(c(-0.4, 1.4)) +
         theme(
           legend.position = c(0.7, 0.9),
           legend.direction = "horizontal",
           plot.caption = element_text(hjust = 0)
         )
       
ggsave(
 "figures/hk-plot1.pdf",
 width = 6.5,
 height = 3.5,
 dpi = 300
 )
       
## Figure 2 ----

dw <- 0.5 # dodge width

dat %>%
  bind_rows(
    tibble(
      panel = "Pre-only",
      lower = -2,
      upper = 2,
      ci_upper = 2,
      ci_lower = -2,
      type = "bounds_pre",
      type2 = "bounds",
    )
  ) %>%
  filter(!(str_detect(type2, "x"))) %>%
    mutate(
    upper_bounds = case_when(str_detect(type2, "bounds") ~ upper),
    lower_bounds = case_when(str_detect(type2, "bounds") ~ lower),
    panel = str_remove(panel, "-only")) %>%
 mutate(
   type2 = fct_relevel(
     type2,
     "bounds",
     "gibbs_d2",
     "bounds_m",
     "gibbs_m_d2",
     "bounds_s",
     "gibbs_s_d2",
     "bounds_ms",
     "gibbs_ms_d2",
     "ols"
   ),
   assumptions = case_when(
     type2 %in% c("gibbs_d2", "bounds") ~ "Randomization",
     type2 %in% c("gibbs_m_d2", "bounds_m") ~ "Randomization\n+ Mod. Monotonicity",
     type2 %in% c("gibbs_s_d2", "bounds_s") ~ "Randomization\n+ Stability",
     type2 %in% c("gibbs_ms_d2", "bounds_ms") ~ "Randomization\n+ Mod. Monotonicity\n+ Stability",
     type2 %in% c("gibbs_ms_d2", "bounds_o") ~ "Randomization\n+ Pri. Monotonicity\n",
     type2 %in% c("gibbs_ms_d2", "bounds_mo") ~ "Randomization\n+ Mod. Monotonicity\n+ Pri. Monotonicity\n",
     type2 %in% c("ols") ~ "OLS"
   ),
   assumptions = factor(
     assumptions,
     levels = c(
       "Randomization",
       "Randomization\n+ Mod. Monotonicity",
       "Randomization\n+ Stability",
       "Randomization\n+ Pri. Monotonicity\n",
       "Randomization\n+ Mod. Monotonicity\n+ Stability",
       "Randomization\n+ Mod. Monotonicity\n+ Pri. Monotonicity\n",
       "OLS"
     )
   ),
   type3 = case_when(
     str_detect(type2, "bounds") ~ "Bounds",
     str_detect(type2, "gibbs") ~ "Gibbs",
     T ~ "OLS"
   )
 ) %>%
 filter(grepl('bounds', type2) | grepl('ols', type2)) %>%
 ggplot(aes(x = panel, color = panel)) +
 facet_grid(cols = vars(assumptions), scales = "free_x") +
 geom_hline(yintercept = 0,
            lty = "dotted",
            color = "red") +
 labs(x = "", y = "") +
 geom_point(aes(y = est), size = 3, position = position_dodge(width = dw)) +
 geom_linerange(aes(ymin = lower, ymax = upper),
                size = 1,
                position = position_dodge(width = dw)) +
 geom_linerange(
   aes(ymin = lower_bounds, ymax = upper_bounds),
   size = 3,
   position = position_dodge(width = dw)
 ) +
 geom_linerange(aes(ymin = ci_lower, ymax = ci_upper),
                size = 1,
                position = position_dodge(width = dw)) +
 scale_linetype_manual(values = c('solid', 'dashed')) +
 guides(
   color = guide_legend(
     override.aes = list(size = 3, shape = NA),
     keyheight = 2
   ),
   linetype = guide_legend(override.aes = list(size = 3), keyheight =
                             2)
 ) +
 labs(#title = "Comparing non-parametric bounds and Bayesian estimates under different assumptions",
   #title = "Y = support candidate; D = land very insecure; T = (T2 or T1) vs control",
   #  subtitle = "X1 = Poor; X2 = Low education; X3 = Over 35",
   color = "", linetype = "") +
 scale_color_manual(values = c("darkred", 'darkblue', "black")) +
 coord_cartesian(ylim = c(-2, 2), clip = "off") +
 theme(
   panel.grid = element_blank(),
   legend.position = "bottom",
   legend.direction = "horizontal",
   plot.margin = unit(c(1, 1, 1, 0), "lines")
 ) +
 guides(color = 'none')

ggsave(
 "figures/hk-plot2-non-param.pdf",
 width = 11,
 height = 4,
 dpi = 300
)

## Figure 3 ----

# Post-test sensitivity analysis

sens_out %>%
 ggplot(aes(x = gamma)) +
 geom_line(aes(y = lower)) +
 geom_line(aes(y = upper)) +
 geom_hline(yintercept = 0, lty = "dotted") +
 geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.4) +
 labs(
   x = expression(
     "Maximum proportion of respondents' moderators affected by treatment" ~  ~ (gamma)
   ),
   y = expression("Interaction effect" ~  ~ (delta))
   #,
   #title = "Sensitivity analysis"
 ) +
 ylim(-2, 2) +
 xlim(0, 0.75)

ggsave(
 "figures/hk-plot-sens.pdf",
 dpi = 300,
 width = 7,
 height = 4
)

## Figure 4 ----

# Pre-test sensitivity analysis

pre_sens_out %>%
 do.call(cbind, .) %>%
 data.frame() %>%
 ggplot(aes(x = thetas)) +
 geom_line(aes(y = lower)) +
 geom_line(aes(y = upper)) +
 geom_hline(yintercept = 0, lty = "dotted") +
 geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.4) +
 labs(
   x = expression("Maximum proportion primed" ~  ~ (theta)),
   y = expression("Interaction effect" ~  ~ (delta))
   #,
   #title = "Sensitivity analysis"
 ) #+
# ylim(-2, 2) #+
#  xlim(0, 0.5)

ggsave(
 "figures/hk-plot-pre-sens.pdf",
 dpi = 300,
 width = 7,
 height = 4
)

## Figure 5 ----

# Randomized placement design sensitivity analysis

prepost_sens_out %>%
 mutate(theta_lab = ifelse(theta == 1, "Unrestricted priming", "Limited priming")) |>
 ggplot(aes(x = gamma)) +
 facet_wrap( ~ theta_lab) +
 geom_line(aes(y = lower)) +
 geom_line(aes(y = upper)) +
 geom_hline(yintercept = 0, lty = "dotted") +
 geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.4) +
 labs(
   x = expression(
     "Maximum proportion of respondents' moderators affected by treatment" ~  ~ (gamma)
   ),
   y = expression("Interaction effect" ~  ~ (delta))
   #,
   #title = "Sensitivity analysis"
 ) +
 ylim(-2, 2) +
 xlim(0, 0.25)

ggsave(
 "figures/hk-plot-prepost-sens.pdf",
 dpi = 300,
 width = 8,
 height = 4
)
           