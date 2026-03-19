################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-20
# Purpose: PLOT COEFFICIENTS FROM NEW AND OLD SCALES, ONLY FOR SCALES WITH DIF
#          FOR ALL ITEMS
# R Version: R version 3.3.3 (2017-03-06)
# Data Input:
#             1. "Data/Derived/2012/AP12-4-Compare_New_and_Old.RData"
#             2. "Data/Derived/2016/AP16-4-Compare_New_and_Old.RData"
# Questions: mpietryka@fsu.edu
################################################################################



# 1.  LOAD PACKAGES ----------------
library(tidyverse) # MANY CONVENIENCE FUNCTIONS
library(stringr)   # STRING FUNCTIONS
library(scales)    # PRETTY SCALES IN PLOTS


# 2. SET FIGURE PREFERENCES -----------------------------

# Sets default colors, labels, etc.
source("Analysis/report-results/Figures/figure_preferences.R")

# Used for labeling the scales, items, groups, etc.
source("DataClean/scale_items.R")

# 3. LOAD DATA  --------------------------------

# 'compare_nested_12' OBJECT CREATED IN 'Analysis\AP12-4-Compare_New_and_Old.R'
load("Data/Derived/2012/AP12-4-Compare_New_and_Old.RData")

# 'compare_nested_16' OBJECT CREATED IN 'Analysis\AP16-4-Compare_New_and_Old.R'
load("Data/Derived/2016/AP16-4-Compare_New_and_Old.RData")


# 4. CLEAN THE DATA ------------------

prepare_data <- function(df) {
  df  %>%
    # REMOVE SCALES WITH ANY VALID ITEMS
    filter(no_good_items == TRUE)  %>%
    # EXTRACT SEs of COEFFICIENTS
    mutate(se_raw = map(lm_raw, "std.error"))  %>%
    mutate(se_ability = map(lm_raw, "std.error"))  %>%
    # CHOOSE RELEVANT VARIABLES
    select(
      scale_name,
      group_var,
      term,
      coef_raw,
      coef_ability,
      se_raw,
      se_ability,
      no_good_items,
      no_dif,
      diff,
      lb_diff,
      ub_diff
    )  %>%
    unnest()  %>%
    # TYPE OF BIAS
    mutate(diff_insig = (sign(lb_diff) != sign(ub_diff)))  %>%
    mutate(magnitude_bias =
             (sign(coef_raw) == sign(coef_ability) &
                diff_insig == FALSE))  %>%
    mutate(sign_bias =
             (sign(coef_raw) != sign(coef_ability) &
                diff_insig == FALSE))  %>%
    mutate(bias_type = "no bias")  %>%
    mutate(bias_type = ifelse(magnitude_bias == TRUE,
                              "magnitude bias", bias_type))  %>%
    mutate(bias_type = ifelse(sign_bias == TRUE,
                              "sign bias", bias_type))  %>%
    # WIDE TO LONG
    gather(model, coef, coef_raw, coef_ability)  %>%
    gather(model_b, se, se_raw, se_ability)   %>%
    separate(model, into = c("source", "model"))  %>%
    separate(model_b, into = c("source_b", "model_b"))   %>%
    filter(model == model_b)  %>%
    select(-c(source, source_b, model_b))  %>%
    # CREATE CONFIDENCE INTERVALS
    mutate(lb = coef - 1.96 * se)  %>%
    mutate(ub = coef + 1.96 * se)  %>%
    mutate(overlaps_zero = (sign(lb) != sign(ub)))  %>%
    # MERGE WITH GROUP VARIABLE LABELS
    left_join(labdata_scale,
              by = c("scale_name" = "scale_abrv"))  %>%
    left_join(group_labs_df,
              by = c("group_var" = "group_abrv", "term" = "term"))  %>%
    left_join(facet_labs, by = c("group_var" = "group_abrv")) %>%
    filter(term != "(Intercept)")    %>%
    mutate_at(vars(value_lab), funs(paste0("[", ., "]")))  %>%
    mutate(value_lab2 = paste(group_lab, anova_id, value_lab))  %>%
    group_by(scale_name, group_var)  %>%
    mutate(no_bias = all(bias_type == "no bias"))
}

plot_data_12 <- prepare_data(compare_nested_12)
plot_data_16 <- prepare_data(compare_nested_16)




# 5. DATA FOR ARROWS ------------------------------
prepare_arrows <- function(df) {
  df  %>%
    filter(diff_insig == FALSE)  %>%
    select(
      group_var,
      facet_lab,
      scale_lab,
      scale_name,
      value_lab2,
      model,
      coef,
      diff_insig,
      bias_type
    )  %>%
    spread(model, coef)
}

arrows_12 <- prepare_arrows(plot_data_12)
arrows_16 <- prepare_arrows(plot_data_16)

# 6. DATA FOR VALUE LABELS ------------------------------
prepare_labels <- function(df){
  label_data <- df %>%
    select(
      group_var,
      facet_lab,
      scale_lab,
      scale_name,
      value_lab,
      value_lab2,
      model,
      coef,
      diff_insig,
      bias_type
    ) %>%
    spread(model, coef)  %>%
    mutate(max_coef = pmax(ability, raw))  %>%
    mutate(min_coef = pmin(ability, raw))  %>%
    mutate(max_moreextreme = abs(max_coef) > abs(min_coef))  %>%
    mutate(label_pos = min_coef)  %>%
    mutate(label_pos = ifelse(min_coef > 0, min_coef, NA)) %>%
    mutate(label_pos = ifelse(max_coef < 0, max_coef, label_pos)) %>%
    mutate(label_pos = ifelse(
      min_coef < 0 & max_coef > 0 &
        max_moreextreme == TRUE,
      min_coef,
      label_pos
    )) %>%
    mutate(label_pos = ifelse(
      min_coef < 0 & max_coef > 0 &
        max_moreextreme == FALSE,
      max_coef,
      label_pos
    )) %>%
    mutate(hjust = ifelse(label_pos > min_coef ,  -.2, 1.2))
}

labels_12 <- prepare_labels(plot_data_12)
labels_16 <- prepare_labels(plot_data_16)

# 7. DATA FOR GROUP LABELS ------------------------------
group_data <- plot_data_12 %>%
  mutate(facet_lab2 = paste0(group_lab, " [ref. = ", baseline, "]" ))  %>%
  ungroup()  %>%
  select(value_lab2, facet_lab, facet_lab2)  %>%
  unique()  %>%
  group_by(facet_lab)  %>%
  filter(row_number() == 1)



# 8. MAKE FIGURE ------------------------------

make_plot <- function(df, arrows, labels, year){
  x_levels <-  df$value_lab2  %>% as.factor()  %>% levels()

  df    %>%
    ggplot(aes(x = value_lab2, y = coef, color = bias_type)) +
    geom_hline(yintercept = 0, linetype = "longdash",  color = "grey50") +
    # PLOT ESTIMATES
    geom_segment(data = arrows,
                 aes(
                   x = value_lab2,
                   y = raw,
                   xend = value_lab2,
                   yend = ability + sign(raw - ability) * .08
                 ),
                 arrow = arrow(length = unit(6, "points")),
                 size = 1) +
    geom_point(aes(shape = model),
               fill = "white",
               size = 2) +
    # LABEL VALUES
    geom_text(data = labels,
              aes(label = value_lab, y = label_pos, hjust = hjust),
              size = rel(2.5),
              show.legend = FALSE
    ) +
    # FORMATTING
    ylab("Difference From Baseline") +
    xlab("") +
    ggtitle(year) +
    facet_wrap( ~ scale_lab, scales = "fixed", ncol = 3, drop = TRUE) +
    coord_flip() +
    theme_minimal(base_size = 8)  +
    scale_shape_manual(name = "Model",
                       values = c(19, 21),
                       breaks = c("raw", "ability"),
                       labels = c("Off-the-shelf", "Corrected"),
                       guide = guide_legend(
                         title.position = "top",
                         title.hjust = 0.5
                       )
    ) +
    scale_color_manual(name = "Type of Bias",
                       values = c(m_color, insig_color, s_color),
                       breaks = c("no bias", "magnitude bias", "sign bias"),
                       labels = c("Insignificant Bias", "Wrong Magnitude", "Wrong Sign"),
                       guide = guide_legend(
                         title.position = "top",
                         title.hjust = 0.5,
                         override.aes = list(
                           size  = 5,
                           shape = 95,
                           linetype = 0,
                           label = "",
                           arrow = ""))
    )   +
    scale_x_discrete(breaks = group_data$value_lab2,
                      labels = group_data$facet_lab2,
                      limits =  rev(x_levels)) +
    scale_y_continuous(breaks= c(-.4, 0, .4), labels = fmt_dcimals(c(1, 0, 1))) +
    expand_limits(y = c(-.8, .7)) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text.x = element_text(angle = 0, vjust = 0),
      strip.text.y = element_text(angle = 0, hjust = 0),
      strip.text = element_text(face="bold", colour=axis_col, size = rel(1)),
      plot.caption = element_text(color = invalid_col),
      plot.subtitle =  element_text(color = axis_col)
    ) +
    theme(legend.position = "bottom")
}

plot_12 <- make_plot(plot_data_12, arrows_12, labels_12, 2012)
plot_16 <- make_plot(plot_data_16, arrows_16, labels_16, 2016)

#+ fig.width=20, fig.height=10

width <- 8
height <- 10
graphics.off()
windows(width, height)
plot_12
windows(width, height)
plot_16



plot_12_2 <- plot_12 +
  theme(legend.position = c(0.75, 0.07),
        legend.justification = 0)







# 9. SAVE FIGURE ------------------------------


ggsave(filename = "invalid_12.pdf",
       plot = plot_12_2,
       width = width,
       height = height)
ggsave(filename = "invalid_12.tiff",
       plot = plot_12_2,
       width = width,
       height = height)

ggsave(filename = "invalid_16.pdf",
       plot = plot_16,
       width = width,
       height = height)
ggsave(filename = "invalid_16.tiff",
       plot = plot_16,
       width = width,
       height = height)

