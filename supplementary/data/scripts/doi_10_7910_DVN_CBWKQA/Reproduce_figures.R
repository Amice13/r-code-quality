
library(tidyverse)
library(cregg)
library(patchwork)

# Data 
conjoint <- read_csv("poq submission/conjoint_data.csv") # Data processing made in conjoint_data.R
conjoint <- conjoint |> mutate(across(where(is.character), as.factor))
conjoint$sexuality_couple <- factor(conjoint$sexuality_couple,
                                    levels = c("Man and woman couple", "Two-women couple", "Two-men couple"))

# General model --------------------

model <- choice ~ sexuality_couple + relationship_couple + age_couple + education_couple + values_couple + reason_couple + pairwise_so_couple
mm <- mm(conjoint, model, id = ~order, weigths = ~weights)

# Figure 1 --------------------

f1 <- mm |> 
  mutate(feature = case_when(
    feature == "pairwise_so_couple" ~ "Couple's sexuality",
    feature == "sexuality_couple" ~ "Couple's sexuality by sex of partners",
    feature == "relationship_couple" ~ "Type of relationship",
    feature == "age_couple" ~ "Couple's age",
    feature == "education_couple" ~ "Couple's education",
    feature == "values_couple" ~ "Couple's family values",
    feature == "reason_couple" ~ "Reason to adopt",),
    feature = factor(feature,
                     levels = c("Couple's sexuality",
                                "Couple's sexuality by sex of partners",
                                "Type of relationship",
                                "Couple's age",
                                "Couple's education",
                                "Couple's family values",
                                "Reason to adopt"))) |> 
  ggplot() +
  geom_vline(xintercept = 0.5, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      group = level,
                      color = feature,
                      xmin = lower, 
                      xmax = upper),
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_x = if_else(mm$estimate > 0.5,0.03,-0.03)) + 
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#DB69A7",
                                "#D91122",
                                "#D56F40",
                                "#FFBC42",
                                "#218380",
                                "#3397E4",
                                "#79137D")) +
  scale_x_continuous(limits = c(0.3,0.7),
                     breaks = c(0.3,0.4,0.5,0.6,0.7),
                     labels = c("0.3","0.4","0.5","0.6","0.7")) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm"),
        strip.placement = "outside",
        panel.spacing.y = unit(0, "lines")) +
  labs(y = "", 
       x = "Marginal means",
       title = "") +
  facet_wrap(vars(feature),
             scales = "free_y",
             ncol = 1,
             strip.position = "left",
             labeller = label_wrap_gen(width=15))

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure 1.tiff",
       plot = f1,
       width = 7,
       height = 6,
       units = "in",
       device = "tiff",
       dpi = 600)

# Figure 2 --------------------

pairw_sex_est <- mm |> 
  filter(feature == "sexuality_couple") |> 
  mutate(level = case_when(
    level == "Man and woman couple" ~ "man_woman",
    level == "Two-women couple" ~ "two_women",
    level == "Two-men couple" ~ "two_men")) |> 
  select(estimate, level) |> 
  pivot_wider(names_from = level, values_from = estimate) |> 
  mutate(diff_gay_het = two_men - man_woman,
         diff_les_het = two_women - man_woman,
         diff_gay_les = two_men - two_women) |> 
  select(diff_gay_het, diff_les_het, diff_gay_les) |> 
  pivot_longer(everything(), names_to = "level", values_to = "estimate") 

pairw_sex_low <- mm |> 
  filter(feature == "sexuality_couple") |> 
  mutate(level = case_when(
    level == "Man and woman couple" ~ "man_woman",
    level == "Two-women couple" ~ "two_women",
    level == "Two-men couple" ~ "two_men")) |> 
  select(lower, level) |> 
  pivot_wider(names_from = level, values_from = lower) |> 
  mutate(diff_gay_het = two_men - man_woman,
         diff_les_het = two_women - man_woman,
         diff_gay_les = two_men - two_women) |> 
  select(diff_gay_het, diff_les_het, diff_gay_les) |> 
  pivot_longer(everything(), names_to = "level", values_to = "lower") 

pairw_sex_up <- mm |> 
  filter(feature == "sexuality_couple") |> 
  mutate(level = case_when(
    level == "Man and woman couple" ~ "man_woman",
    level == "Two-women couple" ~ "two_women",
    level == "Two-men couple" ~ "two_men")) |> 
  select(upper, level) |> 
  pivot_wider(names_from = level, values_from = upper) |> 
  mutate(diff_gay_het = two_men - man_woman,
         diff_les_het = two_women - man_woman,
         diff_gay_les = two_men - two_women) |> 
  select(diff_gay_het, diff_les_het, diff_gay_les) |> 
  pivot_longer(everything(), names_to = "level", values_to = "upper") 

pairw_sex <- left_join(pairw_sex_est, pairw_sex_low)

pairw_sex <- left_join(pairw_sex, pairw_sex_up)

f2 <- pairw_sex |> 
  mutate(level = case_when(
    level == "diff_gay_het" ~ "Two-men vs Man and woman",
    level == "diff_les_het" ~ "Two-women vs Man and woman",
    level == "diff_gay_les" ~ "Two-men vs Two-women"),
    level = factor(level,
                   levels = c("Two-men vs Man and woman",
                              "Two-women vs Man and woman",
                              "Two-men vs Two-women"))) |> 
  ggplot() +
  geom_vline(xintercept = 0, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      group = level,
                      color = level,
                      xmin = lower, 
                      xmax = upper),
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.2) + 
  scale_color_manual(values = c("#D91122",
                                "#D91122",
                                "#D91122")) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.05,0.05),
                     breaks = c(-0.05,-0.025,0.0,0.025,0.05),
                     labels = c("-0.05","-0.025","0.0","0.025","0.05")) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm"),
        strip.placement = "outside",
        panel.spacing.y = unit(0, "lines")) +
  labs(y = "", 
       x = "Marginal means differences",
       title = "") 

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure 2.tiff",
       plot = f2,
       width = 7,
       height = 3,
       units = "in",
       device = "tiff",
       dpi = 600)

# Figure 3 --------------------

sex_rel <- cj(conjoint, choice ~ sexuality_couple, id = ~order, weights = ~weights, estimate = "mm", by = ~ relationship_couple)
diff_sex_rel <- cj(conjoint, choice ~ sexuality_couple, id = ~order, weights = ~weights, estimate = "mm_differences", by = ~ relationship_couple)

sex_rel_plot <- sex_rel |> 
  ggplot() +
  geom_vline(xintercept = 0.5, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      group = BY,
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.2) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.3,0.7),
                     breaks = c(0.3,0.4,0.5,0.6,0.7),
                     labels = c("0.3","0.4","0.5","0.6","0.7")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Marginal means",
       title = "") +
  facet_wrap(vars(BY))

diff_sex_rel_plot <- diff_sex_rel |> 
  ggplot() +
  geom_vline(xintercept = 0, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      group = BY,
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.25) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.3,0.3),
                     breaks = c(-0.2,0,0.2),
                     labels = c("-0.2","0","0.2")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        axis.text.y = element_blank(),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Differences in \nmarginal means",
       title = "") +
  facet_wrap(vars(BY), 
             labeller = label_wrap_gen(width = 25, multi_line = TRUE))

f3 <- sex_rel_plot + diff_sex_rel_plot +
  plot_layout(widths = c(5, 2.5))

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure 3.tiff",
       plot = f3,
       width = 7,
       height = 3,
       units = "in",
       device = "tiff",
       dpi = 600)

# Figure 4 --------------------

sex_gap <- cj(conjoint, choice ~ sexuality_couple, id = ~order, weights = ~weights, estimate = "mm", by = ~ age_couple)
diff_sex_gap <- cj(conjoint, choice ~ sexuality_couple, id = ~order, weights = ~weights, estimate = "mm_differences", by = ~ age_couple)

sex_gap_plot <- sex_gap |> 
  ggplot() +
  geom_vline(xintercept = 0.5, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.2) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.3,0.7),
                     breaks = c(0.3,0.4,0.5,0.6,0.7),
                     labels = c("0.3","0.4","0.5","0.6","0.7")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Marginal means",
       title = "") +
  facet_wrap(vars(BY))

diff_sex_gap_plot <- diff_sex_gap |> 
  ggplot() +
  geom_vline(xintercept = 0, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      group = BY,
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.25) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.3,0.3),
                     breaks = c(-0.2,0,0.2),
                     labels = c("-0.2","0","0.2")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        axis.text.y = element_blank(),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Differences in \nmarginal means",
       title = "") +
  facet_wrap(vars(BY), 
             labeller = label_wrap_gen(width = 30, multi_line = TRUE))

f4 <- sex_gap_plot + diff_sex_gap_plot+
  plot_layout(widths = c(5, 2.5))

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure 4.tiff",
       plot = f4,
       width = 7,
       height = 3,
       units = "in",
       device = "tiff",
       dpi = 600)

# Figure 5 --------------------

sex_ideol <- cj(conjoint, choice ~ sexuality_couple, id = ~order, weights = ~weights, estimate = "mm", by = ~ ideology)

f5 <- sex_ideol |> 
  mutate(BY = case_when(
    BY == "Left" ~ "Left (0-4)",
    BY == "Centre" ~ "Centre (5)",
    BY == "Right" ~ "Right (6-10)"),
    BY = factor(BY,
                levels = c("Left (0-4)",
                           "Centre (5)",
                           "Right (6-10)"))) |> 
  ggplot() +
  geom_vline(xintercept = 0.5, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.2) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.3,0.7),
                     breaks = c(0.3,0.4,0.5,0.6,0.7),
                     labels = c("0.3", "0.4","0.5","0.6","0.7")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Marginal means",
       title = "") +
  facet_wrap(vars(BY))

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure 5.tiff",
       plot = f5,
       width = 7,
       height = 3,
       units = "in",
       device = "tiff",
       dpi = 600)

# Table A1 (in Appendix) --------------------

library(flextable)

flextable(mm |> 
            select(-outcome,
                   -statistic) |>
            mutate(estimate = round(estimate, digits = 2),
                   std.error = round(std.error, digits = 3),
                   z = round(z, digits = 2),
                   lower = round(lower, digits = 2),
                   upper = round(upper, digits = 2),
                   feature = case_when(
                     feature == "pairwise_so_couple" ~ "Couple's sexuality",
                     feature == "sexuality_couple" ~ "Couple's sexuality by sex of partners",
                     feature == "relationship_couple" ~ "Type of relationship",
                     feature == "age_couple" ~ "Couple's age",
                     feature == "education_couple" ~ "Couple's education",
                     feature == "values_couple" ~ "Family values",
                     feature == "reason_couple" ~ "Reason to adopt",),
                   feature = factor(feature,
                                    levels = c("Couple's sexuality",
                                               "Couple's sexuality by sex of partners",
                                               "Type of relationship",
                                               "Couple's age",
                                               "Couple's education",
                                               "Family values",
                                               "Reason to adopt")))) |> 
  border_inner_h(part="all") |> 
  merge_v(j = "feature") |> 
  valign(j = "feature", 
         valign = "top") 

# Figure A1 (in Appendix) --------------------

gender_gap <- cj(conjoint, choice ~ agediff_sex_couple, id = ~order, weights = ~weights, estimate = "mm", by = ~ age_couple)
diff_gender_gap <- cj(conjoint, choice ~ agediff_sex_couple, id = ~order, weights = ~weights, estimate = "mm_difference", by = ~ age_couple)

gender_gap_plot <- gender_gap |> 
  mutate(level = case_when(
    level == "Man has more age than woman" ~ "Man has more\nage than woman",
    level == "Woman has more age than man" ~ "Woman has more\nage than man")) |> 
  ggplot() +
  geom_vline(xintercept = 0.5, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.2) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.4,0.6),
                     breaks = c(0.4,0.5,0.6),
                     labels = c("0.4","0.5","0.6")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Marginal means",
       title = "") +
  facet_wrap(vars(BY))

diff_gender_gap_plot <- diff_gender_gap |> 
  ggplot() +
  geom_vline(xintercept = 0, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      group = BY,
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.2) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.2,0.2),
                     breaks = c(-0.2,0,0.2),
                     labels = c("-0.2","0","0.2")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        axis.text.y = element_blank(),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Differences in \nmarginal means",
       title = "") +
  facet_wrap(vars(BY), 
             labeller = label_wrap_gen(width = 30, multi_line = TRUE))

fa1 <- gender_gap_plot + diff_gender_gap_plot +
  plot_layout(widths = c(5, 2.5))

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure A1.tiff",
       plot = fa1,
       width = 7,
       height = 3,
       units = "in",
       device = "tiff",
       dpi = 600)

# Figure A2 (in Appendix) --------------------

sex_reason <- cj(conjoint, choice ~ sexuality_couple, id = ~order, weights = ~weights, estimate = "mm", by = ~ reason_couple)
diff_sex_reason <- cj(conjoint, choice ~ sexuality_couple, id = ~order, weights = ~weights, estimate = "mm_difference", by = ~ reason_couple)

sex_reason_plot <- sex_reason |> 
  ggplot() +
  geom_vline(xintercept = 0.5, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.2) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.3,0.7),
                     breaks = c(0.3,0.4,0.5,0.6,0.7),
                     labels = c("0.3","0.4","0.5","0.6","0.7")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Marginal means",
       title = "") +
  facet_wrap(vars(BY), 
             labeller = label_wrap_gen(width = 40, multi_line = TRUE))

diff_sex_reason_plot <- diff_sex_reason |> 
  ggplot() +
  geom_vline(xintercept = 0, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      group = BY,
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.2) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(-0.3,0.3),
                     breaks = c(-0.3,0,0.3),
                     labels = c("-0.3","0","0.3")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Differences in marginal means",
       title = "") +
  facet_wrap(vars(BY), 
             labeller = label_wrap_gen(width = 90, multi_line = TRUE))

fa2 <- sex_reason_plot / diff_sex_reason_plot

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure A2.tiff",
       plot = fa2,
       width = 7,
       height = 6,
       units = "in",
       device = "tiff",
       dpi = 600)

# Figure A3 (in Appendix) --------------------

sex_age <- cj(conjoint, choice ~ sexuality_couple, id = ~order, weights = ~weights, estimate = "mm", by = ~ age)

fa3 <- sex_age |> 
  ggplot() +
  geom_vline(xintercept = 0.5, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.3) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.3,0.7),
                     breaks = c(0.3,0.4,0.5,0.6,0.7),
                     labels = c("0.3","0.4","0.5","0.6","0.7")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Marginal means",
       title = "") +
  facet_wrap(vars(BY))

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure A3.tiff",
       plot = fa3,
       width = 7,
       height = 4,
       units = "in",
       device = "tiff",
       dpi = 600)

# Figure A4 (in Appendix) --------------------

rel_age <- cj(conjoint, choice ~ relationship_couple, id = ~order, weights = ~weights, estimate = "mm", by = ~ age)

fa4 <- rel_age |> 
  ggplot() +
  geom_vline(xintercept = 0.5, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.2) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.3,0.7),
                     breaks = c(0.3,0.4,0.5,0.6,0.7),
                     labels = c("0.3","0.4","0.5","0.6","0.7")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Marginal means",
       title = "") +
  facet_wrap(vars(BY))

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure A4.tiff",
       plot = fa4,
       width = 7,
       height = 4,
       units = "in",
       device = "tiff",
       dpi = 600)

# Figure A5 (in Appendix) --------------------

gap_age <- cj(conjoint, choice ~ age_couple, id = ~order, weights = ~weights, estimate = "mm", by = ~ age)

fa5 <- gap_age |> 
  ggplot() +
  geom_vline(xintercept = 0.5, 
             size = 0.3,
             linetype = "dashed") +
  geom_pointrange(aes(y = level,
                      x = estimate, 
                      xmin = lower, 
                      xmax = upper),
                  color = "#D91122",
                  size = 0.2,
                  position = position_dodge(width = 1)) + 
  geom_label(aes(y = level,
                 x = estimate,
                 label = round(estimate, digits = 2),
                 group = level),
             size = 2.75,
             nudge_y = 0.2) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.3,0.7),
                     breaks = c(0.3,0.4,0.5,0.6,0.7),
                     labels = c("0.3","0.4","0.5","0.6","0.7")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 11),
        plot.margin = margin(0,0.5,0.5,0, "cm")) +
  labs(y = "", 
       x = "Marginal means",
       title = "") +
  facet_wrap(vars(BY))

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure A5.tiff",
       plot = fa5,
       width = 7,
       height = 4,
       units = "in",
       device = "tiff",
       dpi = 600)

# Figure S1 (in Supplementary material) --------------------

freq <- cj_freqs(conjoint, model, id = ~order)

fs1 <- freq |> 
  mutate(feature = case_when(
    feature == "pairwise_so_couple" ~ "Couple's sexuality",
    feature == "sexuality_couple" ~ "Couple's sexuality by sex of partners",
    feature == "relationship_couple" ~ "Type of relationship",
    feature == "age_couple" ~ "Couple's age",
    feature == "education_couple" ~ "Couple's education",
    feature == "values_couple" ~ "Couple's family values",
    feature == "reason_couple" ~ "Reason to adopt",),
    feature = factor(feature,
                     levels = c("Couple's sexuality",
                                "Couple's sexuality by sex of partners",
                                "Type of relationship",
                                "Couple's age",
                                "Couple's education",
                                "Couple's family values",
                                "Reason to adopt"))) |> 
  ggplot() +
  geom_col(aes(y = level,
               x = estimate, 
               group = level,
               fill = feature)) + 
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("#DB69A7",
                               "#D91122",
                               "#D56F40",
                               "#FFBC42",
                               "#218380",
                               "#3397E4",
                               "#79137D")) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "white"),
        text = element_text(size = 13),
        plot.margin = margin(0,0.5,0.5,0, "cm"),
        strip.placement = "outside",
        panel.spacing.y = unit(0, "lines")) +
  labs(y = "", 
       x = "Frequencies",
       title = "") +
  facet_wrap(vars(feature),
             scales = "free_y",
             ncol = 1,
             strip.position = "left",
             labeller = label_wrap_gen(width=15))

ggsave("poq submission/figures/Lopez Ortega 24-0076.R2 Figure S1.tiff",
       plot = fs1,
       width = 10,
       height = 8,
       units = "in",
       device = "tiff",
       dpi = 600)
