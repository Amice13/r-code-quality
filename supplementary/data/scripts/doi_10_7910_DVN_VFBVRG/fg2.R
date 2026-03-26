################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-15
# Purpose: PLOT P-VALUES FROM DIF TESTS FOR EGALITARIANSIM SCALE
# R Version: R version 3.3.3 (2017-03-06)
# Data Input:
#             1. "Data/Derived/2012/AP12-3-DIF.RData"
#             2. "Data/Derived/2016/AP16-3-DIF.RData"
#
# Questions: mpietryka@fsu.edu
################################################################################

plot_name <- "fg2"


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




# 4.  CLEAN DATA   --------------------------------



# FUNCTION TO CLEAN DATA
dif_nested <- dif_nested_12  %>%
  mutate(year = 2012L)  %>%
  bind_rows(dif_nested_16)  %>%
  mutate(year = ifelse(is.na(year), 2016L, year)) %>%
  # EXTRACT ANOVAS
  select(scale_name, group_var, anova, no_good_items, year)  %>%
  unnest() %>%
  # REMOVE ROWS FOR RESIDUALS
  filter(term != "Residuals")  %>%
  group_by(scale_name, group_var, item, year)  %>%
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
  left_join(labdata_scale, by = c("scale_name" = "scale_abrv"))







# RESTRICT TO FOCAL SCALE, GROUPING VARIABLE

focal_scales <- c("egal_vars")
focal_groups <- c("educ_ordinal", "income")

focal_plotdata <- dif_nested  %>%
  filter(group_var %in% focal_groups)  %>%
  filter(scale_name %in% focal_scales)


# 5. ITEM LABELS   --------------------------------



focal_plotdata  <- focal_plotdata   %>%
  mutate(value_lab = recode(
    item,
    "egal_equal" = "Society should do whatever necessary to make sure everyone has equal opportunity",
    "egal_worryless" = "Country would be better off if we worried less about how equal people are",
    "egal_notbigprob" = "Not big problem if some people have more of a chance in life",
    "egal_fewerprobs" = "If people were treated more equally we would have fewer problems"
    ))  %>%
  mutate(value_lab = stringr::str_wrap(value_lab, width = 45, exdent = 4))  %>%
  arrange(item_id)

# ORDER FOR X VALUES
x_levels <- with(focal_plotdata,
                 forcats::fct_reorder(value_lab, item_id, .desc = TRUE))  %>%
  levels()




# FACET ANNOTATION

facet_df <- focal_plotdata  %>%
  filter(group_var == "educ_ordinal")  %>%
  group_by(year)  %>%
  filter(row_number() == 1)  %>%
  mutate(lab = recode(
    year,
    `2012` = stringr::str_wrap("Only item 2 is valid", width = 10),
    `2016` = "No valid items"
  ))  %>%
  mutate(lab_x = recode(year,
                        `2012` = 2.5,
                        `2016` = 2.5))  %>%
  mutate(lab_y = recode(year,
                        `2012` = 0.35,
                        `2016` = 0.1))



# 6. CREATE PLOT  --------------------------------


# The plot shows p-values from the final anova in which it was included unsplit


# OVERRIDE INSIG COLOR
is_col <- "grey60"


# FUNCTION TO CREATE PLOT
plot_pvals <- function(df, show_leg = TRUE, show_scale = TRUE){
  the_plot <- df  %>%
    ggplot(aes(x = value_lab, y = bh_pval, color = factor(ss))) +
    # HIGHLIGHT BOXES WITH NO GOOD ITEMS
    geom_rect(data = filter(df, no_good_items == TRUE)  %>%
                group_by(group_var, scale_name, year) %>%
                filter(row_number()==1),
              fill = invalid_col,
              color = invalid_col,
              xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,
              alpha = 0.5) +
    # HORIZONTAL LINE TO INDICATE p = 0.05
    geom_hline(yintercept = 0.05,
               size = 0.5,
               linetype = "longdash",
               color = axis_col) +
    # ADD POINTS, LINES
    geom_linerange(aes(ymin = -Inf, ymax = bh_pval),
                   linetype = "solid", size = 1) +
    geom_point(size = 6) +
    # LABEL POINTS
    geom_text(aes(label = item_id),
              fontface = "bold", color = "white", size = rel(4)) +
    # LABEL FACETS
    geom_text(data = facet_df, aes(label = lab, x = lab_x, y = lab_y),
              color = c(is_col, ss_col),
              size = rel(3.3),
              fontface = "bold",
              hjust = 0) +
    # FORMATTING
    facet_wrap( ~ group_lab + year , scales = "free_x", nrow = 1) +
    xlab("") +
    ylab("BH-corrected p-value") +
    labs(title = "",
         caption = "Low p-values indicate differential item functioning") +
    scale_colour_manual(values = c(is_col, ss_col),
                        labels=c(expression("p " >= " .05"),
                                 expression("p " < " .05"))
                                 ) +
    scale_y_continuous(
      limits = c(-.05, .93),
      breaks = c(".05" = 0.05, ".4" = 0.4, ".8" = 0.8),
      minor_breaks = NULL
      ) +
    scale_x_discrete(limits = x_levels) +
    theme_light(base_size = 15)  +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y= element_text(size = rel(.8)),
          strip.text.x = element_text(angle = 0, vjust = 0),
          strip.text.y = element_text(angle = 0, hjust = 0),
          strip.text = element_text(face="bold", colour=axis_col),
          strip.background = element_rect(fill="white", color=NULL),
          plot.caption = element_text(color = axis_col),
          plot.subtitle =  element_text(color = axis_col)
    ) +
    coord_flip() +
  theme(legend.position = "none")


  return(the_plot)
  }


pvals_plot <- plot_pvals(focal_plotdata)


width  <- 8
height <- 4
graphics.off()
windows(width, height)
#+ fig.width=12, fig.height=10
pvals_plot




# 7. SAVE  --------------------------------


# old name: egal_p-values
ggsave(filename = paste0(plot_name, ".pdf"),
       plot = pvals_plot,
       width = width,
       height = height)

ggsave(filename = paste0(plot_name, ".tiff"),
       dpi = 800,
       plot = pvals_plot,
       width = width,
       height = height)


