################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-20
# Purpose: Plot egalitarianism coefficients for education and income
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: "Data/Derived/2012/AP12-4-Compare_New_and_Old.RData"
#
# Questions: mpietryka@fsu.edu
################################################################################

plot_name <- "fg4b"





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



# 4. COMBINE YEARS   --------------------------------



# RESTRICT TO FOCAL SCALE, GROUPING VARIABLE
focal_scales <- c("egal_vars")
focal_groups <- c("educ_ordinal", "income")

df <- compare_nested_12  %>%
  filter(group_var %in% focal_groups)  %>%
  filter(scale_name %in% focal_scales)




# 5. CLEAN DATA TO PLOT THE DIFFERENCES B/W MODELS ----------------------
plot_colors <- c(m_color, insig_color, s_color)

plot_data <- df  %>%
  # EXTRACT SEs of COEFFICIENTS
  mutate(se_raw = map(lm_raw, "std.error"))  %>%
  mutate(se_ability = map(lm_raw, "std.error"))  %>%
  # CHOOSE RELEVANT VARIABLES
  select(scale_name,
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
         ub_diff)  %>%
  unnest()  %>%
  # TYPE OF BIAS
  mutate(diff_insig = ( sign(lb_diff) != sign(ub_diff)))  %>%
  mutate(magnitude_bias =
           ( sign(coef_raw) == sign(coef_ability) & diff_insig == FALSE))  %>%
  mutate(sign_bias =
           ( sign(coef_raw) != sign(coef_ability) & diff_insig == FALSE))  %>%
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
  mutate(overlaps_zero = ( sign(lb) != sign(ub)))  %>%
  # REMOVE SCALES WITH NO GOOD ITEMS
  mutate_at(vars(coef, lb, ub, overlaps_zero),
            funs(ifelse(no_good_items == FALSE, ., NA))) %>%
  # MERGE WITH GROUP VARIABLE LABELS
  left_join(group_labs_df,
            by = c("group_var" = "group_abrv", "term" = "term"))  %>%
  left_join(facet_labs, by = c("group_var" = "group_abrv")) %>%
  filter(term != "(Intercept)")    %>%
  group_by(scale_name, group_var)  %>%
  mutate(no_bias = all(bias_type == "no bias"))





# 6. FACET LABELS  ----------------------

facet_data <- plot_data  %>%
  filter(group_var == "educ_ordinal")  %>%
  mutate(label = recode(
    value_lab,
    "Grad. deg." = 'paste("Estimates ", bold("differ in magnitude"))',
    "Bachelor's" = 'paste("Estimates in ", bold("opposite directions"))',
    "Some college" = 'paste(bold("No significant difference"), "b/w estimates")',
    .default = NA_character_))  %>%
  filter(!is.na(label) & model == "raw")






# 7. MAKE FIGURE ----------------------

dodge_width <- 0.4

coef_plot <- plot_data    %>%
  ggplot(aes(x = value_lab, y = coef, color = bias_type, shape = model)) +
    geom_hline(yintercept = 0, linetype = "longdash",  color = "grey50") +
    # PLOT ESTIMATES
    geom_linerange( aes(ymin = lb, ymax = ub),
      position =  position_dodge(width = dodge_width),
      linetype = "solid",
      size = 1.5) +
    geom_point( fill = "white",
                   position =  position_dodge(width = dodge_width),
                   size = 5) +
    # LABELS
    geom_text(data =  facet_data,
              y = 0,
              aes(label = label),
              vjust = c(2.5, 2, 2),
              size = 4,
              parse = TRUE) +
    # FORMATTING
    ylab("Difference From Baseline") +
    xlab("") +
    facet_wrap( ~ facet_lab, scales = "free") +
    coord_flip() +
    theme_light(base_size = 16)  +
  scale_shape_manual(name = "Model",
                     values = c(19, 21),
                     breaks = c("raw", "ability"),
                     labels = c("Off-the-shelf", "Corrected")
  ) +
  scale_color_manual(
    name = "Type of Bias",
    values = c(m_color, insig_color, s_color),
    breaks = c("no bias", "magnitude bias", "sign bias"),
    labels = c("Insignificant Bias", "Wrong Magnitude", "Wrong Sign"),
    guide = guide_legend(
      override.aes =  list(linetype = 1,
                           label = "",
                           shape = "",
                           size = 2,
                           lineend="butt"))
    ) +
  scale_y_continuous(breaks= pretty_breaks(n = 3)) +
  theme(strip.text.y = element_text(angle = 0)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face="bold", colour= axis_col),
    strip.background = element_rect(fill = "white", colour = NULL),
    plot.caption = element_text(color = axis_col),
    plot.subtitle =  element_text(color = axis_col)
  ) +
  theme(legend.position = "bottom")




width  <- 11
height <- 5
graphics.off()
windows(width, height)
#+ fig.width=12, fig.height=10
coef_plot





# 8. SAVE ----------------------


ggsave(filename = paste0(plot_name, ".pdf"),
       plot = coef_plot,
       width = width,
       height = height)
ggsave(filename = paste0(plot_name, ".tiff"),
       dpi = 800,
       plot = coef_plot,
       width = width,
       height = height)


