library(tidyverse)
library(forcats)

theme_set(theme_bw())

# load results
load('output/hk-replication.Rdata')

dw <- 0.5 # set dodge width for plot


dat %>%
  filter(panel != "Pre-only",!(str_detect(type2, "x")),!(type2 %in% c("bounds_mo", "bounds_o"))) %>%
  mutate(
    upper_bounds = case_when(str_detect(type2, "bounds") ~ upper),
    lower_bounds = case_when(str_detect(type2, "bounds") ~ lower)
  ) %>%
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
      type2 %in% c("gibbs_m_d2", "bounds_m") ~ "Randomization\n+ Monotonicity",
      type2 %in% c("gibbs_s_d2", "bounds_s") ~ "Randomization\n+ Stability",
      type2 %in% c("gibbs_ms_d2", "bounds_ms") ~ "Randomization\n+ Monotonicity\n+ Stability",
      type2 %in% c("ols") ~ "OLS"
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
    )) %>%
   mutate(type3 = case_when(
      str_detect(type2, "bounds") ~ "Bounds",
      str_detect(type2, "gibbs") ~ "Gibbs",
      T ~ "OLS"
    )
  ) %>%
  ggplot(aes(x = type3,
             color = panel)) +
  facet_grid(cols = vars(assumptions),rows=vars(panel), scales = "free_x") +
  geom_hline(yintercept = 0,
             lty = "dotted",
             color = "red") +
  labs(x = "", y = "") +
  geom_point(aes(y = est),
             size = 3,
             position = position_dodge(width = dw)) +
  geom_linerange(
    aes(ymin = lower, ymax = upper, linetype=type3),
    size = 1,
    position = position_dodge(width = dw)
  ) +
  geom_linerange(
    aes(ymin = lower_bounds, ymax = upper_bounds),
    size = 3,
    position = position_dodge(width = dw)
  ) +
  geom_linerange(
    aes(ymin = ci_lower, ymax = ci_upper),
    size = 1,
    position = position_dodge(width = dw)
  ) +
  scale_linetype_manual(values=c('solid','dashed','dotted'))+
  guides(color = guide_legend(override.aes = list(size = 3, shape = NA),keyheight=2),
         linetype=guide_legend(override.aes = list(size = 3),keyheight=2)) +
  labs(#title = "Comparing non-parametric bounds and Bayesian estimates under different assumptions",
    #title = "Y = support candidate; D = land very insecure; T = (T2 or T1) vs control",
    #  subtitle = "X1 = Poor; X2 = Low education; X3 = Over 35",
    color = "",
    linetype = "") +
  scale_color_manual(values = c("grey", "black")) +
  coord_cartesian(ylim = c(-1.8, 1.8), clip = "off") +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.margin = unit(c(1, 1, 1, 0), "lines")
  ) +
  scale_x_discrete(breaks = NULL)


ggsave("figures/hk-plot2.pdf", width = 8, height = 6,dpi=300)

