################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-15
# Purpose: Egalitarianism item locations as education and income vary
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: "Data/Derived/2012/AP12-3-DIF.RData"
#
# Questions: mpietryka@fsu.edu
################################################################################

plot_name <- "fg4a"

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

# 'dif_nested_12' OBJECT CREATED IN 'Analysis\AP12-3-DIF.R'
load("Data/Derived/2012/AP12-3-DIF.RData")


#  4. COMPARE MODEL RESULTS   --------------------------------


# RESTRICT TO FOCAL SCALE, GROUPING VARIABLE

focal_scales <- c("egal_vars")
focal_groups <- c("educ_ordinal", "income")


focal_df <- dif_nested_12  %>%
  filter(group_var %in% focal_groups)  %>%
  filter(scale_name %in% focal_scales)



# 5. CLEAN BASELINE MODEL DATA ----------------


# FUNCTION
clean_baseline <- function(mod, group){
 mod$item  %>%
    arrange(item)  %>%
    tbl_df()  %>%
    # MERGE WITH GROUP VARIABLE LABELS
    mutate(group_var = group)  %>%
    mutate(scale_name = "egal_vars")  %>%
    left_join(labdata_group, by = c("group_var" = "group_abrv"))  %>%
    left_join(labdata_scale, by = c("scale_name" = "scale_abrv"))  %>%
    left_join(item_ids, by = c("scale_name" = "scale_abrv", "item"))  %>%
    # ARRANGE
    select(group_lab, scale_lab, item, item_id, xsi.item)  %>%
    arrange(item_id)
}


# EXECUTE
focal_df <- focal_df  %>%
  mutate(baseline_df = map2(mod1, group_var, clean_baseline))

# 6. CLEAN FINAL MODEL DATA ----------------


# FUNCTION
clean_final <- function(mod, group){
  final_df <- mod$item %>%
    arrange(item)  %>%
    tbl_df()  %>%
    # IDENTIFY GROUPS COMBINED IN ITEM
    mutate(split_groups = str_extract(item, "[0-9]+") )    %>%
    mutate(split_groups = recode(split_groups, .missing = "01234"))    %>%
    mutate(split_groups = gsub("(.{1})", "\\1 ", split_groups))   %>%
    separate(split_groups , paste0("value", 1:5), sep = " " )  %>%
    mutate_at(vars(starts_with("value")), as.numeric)  %>%
    select_if(function(col) (unique(col)  %>% length()) > 1)  %>%
    # REMOVE SPLIT IDs FROM ITEM NAME
    mutate(word1 = word(item, 1, sep = "_"))  %>%
    mutate(word2 = word(item, 2, sep = "_"))  %>%
    mutate(item = paste(word1, word2, sep = "_"))   %>%
    # MERGE WITH GROUP VARIABLE LABELS
    mutate(group_var = group)  %>%
    mutate(scale_name = "egal_vars")  %>%
    left_join(labdata_group, by = c("group_var" = "group_abrv"))  %>%
    left_join(labdata_scale, by = c("scale_name" = "scale_abrv"))  %>%
    left_join(item_ids, by = c("scale_name" = "scale_abrv", "item"))  %>%
    # ARRANGE
    select(group_lab, scale_lab, item, item_id, xsi.item, value1:value5)  %>%
    arrange(item_id) %>%
    # WIDE TO LONG
    gather(order, anova_id, value1:value5)  %>%
    left_join(group_labs_df, by = c("group_lab", "anova_id"))  %>%
    filter(!is.na(value_lab))
}



# EXECUTE
focal_df <- focal_df  %>%
  mutate(final_df = map2(mod_final, group_var, clean_final))

plot_baseline <- focal_df$baseline_df  %>% bind_rows() %>%
  mutate(max_x = ifelse(group_lab == "Education", 5L, 3L))

plot_final <- focal_df$final_df  %>%
  bind_rows()  %>%
  mutate(max_x = ifelse(group_lab == "Education", 5L, 3L))






# 7. MAKE PLOT  --------------------------------




the_plot <- ggplot(plot_final,
       aes(
         x = factor(value_lab),
         y = xsi.item,
         group = factor(item_id),
         label = factor(item_id),
         color = factor(item_id)
       )) +
  # FACETS
  facet_wrap(~ group_lab, scales = "free_x", ncol = 2) +
  # DISPLAY CORRECTED ESTIMATES
  geom_line(size = 2) +
  # DISPLAY AND LABEL BASELINE LOCATIONS
  geom_segment(data = plot_baseline,
               aes(x = 1, xend = max_x, yend = xsi.item),
               linetype = "longdash",
               size = 1.5) +
  geom_text(data = filter(plot_baseline, group_lab == "Education"),
            x = 1,
            vjust = c(0.5, 0.5, 0.2, 0.8),
            hjust = 1.1,
            aes(label = paste("Baseline Item", item_id)),
            size = rel(4)) +
  geom_point(size = 6) +
  # LABEL POINTS WITH ITEM
  geom_text(fontface = "bold", color = "white", size = rel(5)) +
  labs(x = "",
       y = "Location") +
  scale_colour_brewer(palette = "Set1") +
  scale_y_continuous(
    breaks = scales::pretty_breaks(4),
    minor_breaks = NULL
  ) +
  scale_x_discrete(expand = c(0.3, 0.3)) +
  theme_minimal(base_size = 16)  +
  theme(
        panel.grid.minor = element_blank(),
        axis.text.x= element_text(angle = 45, hjust = 1),
        strip.text = element_text(face="bold", colour= NULL),
        strip.background = element_rect(fill="white", colour="white"),
        plot.caption = element_text(color = axis_col),
        plot.subtitle =  element_text(color = axis_col)
  ) +
  theme(legend.position = "none")



width  <- 14
height <- 6
graphics.off()
windows(width, height)
#+ fig.width=12, fig.height=10
the_plot




# 8. SAVE  --------------------------------


ggsave(filename = paste0(plot_name, ".pdf"),
       plot = the_plot,
       width = width,
       height = height)
ggsave(filename = paste0(plot_name, ".tiff"),
       dpi = 800,
       plot = the_plot,
       width = width,
       height = height)


