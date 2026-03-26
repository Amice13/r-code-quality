################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-15
# Purpose: Grid of DIF tests for all scales and grouping variables
# R Version: R version 3.3.3 (2017-03-06)
# Data Input:
#             1. "Data/Derived/2012/AP12-3-DIF.RData"
#             2. "Data/Derived/2016/AP16-3-DIF.RData"
# Questions: mpietryka@fsu.edu
################################################################################

plot_name <- "fg3"

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

# 'dif_nested_16' OBJECT CREATED IN 'Analysis\AP16-3-DIF.R'
load("Data/Derived/2016/AP16-3-DIF.RData")





#  4. CLEAN DATA   --------------------------------



#  FUNCTION TO CLEAN DATA
prep_pvalues <- function(df, df_year){
  extracted_anovas <- df  %>%
    # EXTRACT ANOVAS
    select(scale_name, group_var, anova, no_good_items)  %>%
    unnest() %>%
    # REMOVE ROWS FOR RESIDUALS
    filter(term != "Residuals")  %>%
    group_by(scale_name, group_var, item)  %>%
    # KEEP ONLY FINAL P-VALUES (DROP P-VALUES OF PRIOR ITERATIONS)
    mutate(last_run = ifelse(run == max(run), 1, 0))  %>%
    filter(last_run == 1)  %>%
    # KEEP ONLY ORIGINAL ITEMS (DROP PSEUDO ITEMS)
    filter(item %in% get(scale_name))  %>%
    # ITEM ID
    left_join(item_ids, by = c("scale_name" = "scale_abrv", "item"))  %>%
    # Is statistically significant?
    mutate(ss = ifelse(bh_pval < .05, 1, 0) )  %>%
    # MERGE WITH GROUP VARIABLE LABELS
    left_join(labdata_group, by = c("group_var" = "group_abrv"))  %>%
    left_join(labdata_scale, by = c("scale_name" = "scale_abrv"))  %>%
    # ADD YEAR VARIABLE
    mutate(year = df_year)

  return(extracted_anovas)
  }

anova_plotdata_12 <- prep_pvalues(dif_nested_12, df_year = 2012)
anova_plotdata_16 <- prep_pvalues(dif_nested_16, df_year = 2016)



# 5. DATA TO HELP WITH FIGURE FORMAT   --------------------------------


#  DATA TO ORDER FACETS

scale_data <- anova_plotdata_12  %>%
  group_by(scale_lab)  %>%
  summarise(prop_bad = sum(no_good_items)/n())  %>%
  arrange(-prop_bad)
group_data <- anova_plotdata_12  %>%
  group_by(group_lab, scale_lab)  %>%
  mutate(any_bad = any(no_good_items))  %>%
  select(group_lab, any_bad, scale_lab)  %>%
  unique()  %>%
  group_by(group_lab)  %>%
  summarise(prop_bad = sum(any_bad)/n())  %>%
  arrange(-prop_bad)

stacked_plotdata <- anova_plotdata_12  %>%
  bind_rows(anova_plotdata_16)  %>%
  mutate(scale_lab_r = factor(scale_lab, levels = scale_data$scale_lab))  %>%
  mutate(group_lab_r = factor(group_lab, levels = rev(group_data$group_lab)))






# 6. CREATE PLOT   --------------------------------


# The plot shows p-values from the final anova in which it was included unsplit


df <- stacked_plotdata

# OVERRIDE INSIG COLOR
is_col <- "grey80"


# FUNCTION TO CREATE PLOT
  the_plot <- df  %>%
    ggplot(aes(x = as.integer(item_id), y = group_lab_r, fill = factor(ss))) +
    geom_tile(colour = "white") +
    geom_point(data = filter(df, no_good_items == TRUE)  %>%
                 group_by(scale_name, group_var, year) %>%
                 mutate(pos = max(item_id) + 1)  %>%
                 filter(row_number()==1),
               x = 0.2,
               shape = "x",
               fill = x_col,
               size = 5,
               color = x_col) +
    scale_fill_manual(values = c(is_col, ss_col),
                      labels=c("Insignificant", "DIF")
                      )  +
  facet_grid(year ~ scale_lab_r, scales = "free_x", space = "free_x") +
    labs(title = "",
       subtitle = NULL,
       caption = "X = Invalid scale; All items show DIF",
       x = "Item #",
       y = "") +
    theme_light(base_size = 15)  +
    expand_limits(x = 0.01) +
    scale_x_continuous(breaks= seq_len(10)) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text.x = element_text(angle = 90, vjust = 0),
          strip.text.y = element_text(angle = 0, hjust = 0),
          strip.text = element_text(face="bold", colour=axis_col),
          strip.background = element_rect(colour  = NA, fill=NA),
          plot.caption = element_text(color = x_col, vjust = 15),
          plot.subtitle =  element_text(color = axis_col)
    )


    the_plot


width  <- 12
height <- 8
graphics.off()
windows(width, height)
the_plot

# 7. SAVE PLOT   --------------------------------

ggsave(filename = paste0(plot_name, ".pdf"),
       plot = the_plot,
       width = width,
       height = height)
ggsave(filename = paste0(plot_name, ".tiff"),
       dpi = 800,
       plot = the_plot,
       width = width,
       height = height)

