load('output/hk-replication.Rdata')

library(tidyverse)
theme_set(theme_bw())

dw <- 0.8

dat$width <- dat$upper - dat$lower

w_r_shrink <-
  (dat$width[dat$type == "gibbs_post_x_d2"] - dat$width[dat$type == "gibbs_post_d2"]) / dat$width[dat$type == "gibbs_post_d2"]
w_rm_shrink <-
  (dat$width[dat$type == "gibbs_post_mx_d2"] - dat$width[dat$type == "gibbs_post_m_d2"]) / dat$width[dat$type == "gibbs_post_m_d2"]
w_rs_shrink <-
  (dat$width[dat$type == "gibbs_post_sx_d2"] - dat$width[dat$type == "gibbs_post_s_d2"]) / dat$width[dat$type == "gibbs_post_s_d2"]
w_rms_shrink <-
  (dat$width[dat$type == "gibbs_post_msx_d2"] - dat$width[dat$type == "gibbs_post_ms_d2"]) / dat$width[dat$type == "gibbs_post_ms_d2"]
w_ols_shrink <-
  (dat$width[dat$type == "ols_post_x"] - dat$width[dat$type == "ols_post"]) / dat$width[dat$type == "ols_post"]

w_r_shrink2 <-
  (dat$width[dat$type == "gibbs_prepost_x_d2"] - dat$width[dat$type == "gibbs_prepost_d2"]) / dat$width[dat$type == "gibbs_prepost_d2"]
w_rm_shrink2 <-
  (dat$width[dat$type == "gibbs_prepost_mx_d2"] - dat$width[dat$type == "gibbs_prepost_m_d2"]) / dat$width[dat$type == "gibbs_prepost_m_d2"]
w_rs_shrink2 <-
  (dat$width[dat$type == "gibbs_prepost_sx_d2"] - dat$width[dat$type == "gibbs_prepost_s_d2"]) / dat$width[dat$type == "gibbs_prepost_s_d2"]
w_rms_shrink2 <-
  (dat$width[dat$type == "gibbs_prepost_msx_d2"] - dat$width[dat$type == "gibbs_prepost_ms_d2"]) / dat$width[dat$type == "gibbs_prepost_ms_d2"]
w_ols_shrink2 <-
  (dat$width[dat$type == "ols_prepost_x"] - dat$width[dat$type == "ols_prepost"]) / dat$width[dat$type == "ols_prepost"]

dat_labels = data.frame(x = rep(1.8, 5),
                        y = rep(-1, 5),
                        assumptions = factor(
                          c(
                            "Randomization",
                            "Randomization\n+ Monotonicity",
                            "Randomization\n+ Stability",
                            "Randomization\n+ Monotonicity\n+ Stability",
                            "OLS"
                          ),
                          levels = c(
                            "Randomization",
                            "Randomization\n+ Monotonicity",
                            "Randomization\n+ Stability",
                            "Randomization\n+ Monotonicity\n+ Stability",
                            "OLS"
                          )
                        ),
                        label = paste0(
                          format(
                          c(
                            w_r_shrink,
                            w_rm_shrink,
                            w_rs_shrink,
                            w_rms_shrink,
                            w_ols_shrink
                          )*100,
                          digits = 1
                        ), "%")
)

dat_labels2 = data.frame(x = rep(2.2, 5),
                        y = rep(-0.8, 5),
                        assumptions = factor(
                          c(
                            "Randomization",
                            "Randomization\n+ Monotonicity",
                            "Randomization\n+ Stability",
                            "Randomization\n+ Monotonicity\n+ Stability",
                            "OLS"
                          ),
                          levels = c(
                            "Randomization",
                            "Randomization\n+ Monotonicity",
                            "Randomization\n+ Stability",
                            "Randomization\n+ Monotonicity\n+ Stability",
                            "OLS"
                          )
                        ),
                        label = paste0(
                          format(
                            c(
                              w_r_shrink2,
                              w_rm_shrink2,
                              w_rs_shrink2,
                              w_rms_shrink2,
                              w_ols_shrink2
                            )*100,
                            digits = 1
                          ), "%")
)

p <- dat %>%
  filter(panel != "Pre-only",
         (str_detect(type2, "gibbs") | str_detect(type2, "ols"))) %>%
  mutate(
    upper_bounds = case_when(str_detect(type2, "bounds") ~ upper),
    lower_bounds = case_when(str_detect(type2, "bounds") ~ lower),
    covars = ifelse(str_detect(type2, "x"), "Covariates", "No covariates"),
    covars = fct_relevel(covars, "No covariates", "Covariates")
  ) %>%
  mutate(
    type2 = fct_relevel(
      type2,
      "gibbs_d2",
      "gibbs_x_d2",
      "gibbs_m_d2",
      "gibbs_mx_d2",
      "gibbs_s_d2",
      "gibbs_sx_d2",
      "gibbs_ms_d2",
      "gibbs_msx_d2",
      "ols",
      "ols_x"
    ),
    assumptions = case_when(
      type2 %in% c("gibbs_d2", "gibbs_x_d2") ~ "Randomization",
      type2 %in% c("gibbs_m_d2", "gibbs_mx_d2") ~ "Randomization\n+ Monotonicity",
      type2 %in% c("gibbs_s_d2", "gibbs_sx_d2") ~ "Randomization\n+ Stability",
      type2 %in% c("gibbs_ms_d2", "gibbs_msx_d2") ~ "Randomization\n+ Monotonicity\n+ Stability",
      type2 %in% c("ols", "ols_x") ~ "OLS"
    ),
    assumptions = factor(
      assumptions,
      levels = c(
        "Randomization",
        "Randomization\n+ Monotonicity",
        "Randomization\n+ Stability",
        "Randomization\n+ Monotonicity\n+ Stability",
        "OLS"
      )
    )
  ) %>%
  ggplot(aes(x = covars,
             color = panel,
             shape = covars)) +
  facet_grid(cols = vars(assumptions)) +
  geom_hline(yintercept = 0,
             lty = "dotted",
             color = "red") +
  labs(x = "", y = "") +
  geom_point(aes(y = est), size = 3,
             position = position_dodge(width = dw)) +
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
  guides(color = guide_legend(override.aes = list(size = 3, shape = NA))) +
  labs(#title = "Comparing Bayesian estimates under different assumptions,\nwith and without covariates",
    #title = "Y = support candidate; D = land very insecure; T = (T2 or T1) vs control",
    # subtitle = "X1 = Poor; X2 = Low education; X3 = Over 35",
    color = "", shape = "") +
  scale_x_discrete(breaks = NULL) +
  scale_color_manual(values = c("grey", "black")) +
  coord_cartesian(ylim = c(-1.3, 1.7),
                  clip = "off",
                  #xlim = c(0.5, 10.5),
                  expand = FALSE) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    plot.margin = unit(c(1, 1, 1, 0), "lines")
  )


p +
  geom_text(mapping = aes(x = x, y = y, label = label),
              data = dat_labels,
              size = 2.3, color = "grey",
              inherit.aes = FALSE) +
  geom_text(mapping = aes(x = x, y = y, label = label),
            data = dat_labels2,
            size = 2.3, color = "black",
            inherit.aes = FALSE)

# +
#   annotate(
#     geom = "text",
#     x = c(1.5, 3.5, 5.5, 7.5, 9.5),
#     y = -1.4,
#     vjust = 1,
#     label = c("Randomization", "R + Monotonicity", "R + Stability", "R + Monotonicity\n+ Stability", "OLS")
#   ) +
#   annotate(
#     geom = "linerange",
#     x = c(2.5, 4.5, 6.5, 8.5),
#     ymin = -1.4, ymax = -1.2
#   )

ggsave("figures/hk-plot3.pdf", width = 8, height = 4,dpi=300)
