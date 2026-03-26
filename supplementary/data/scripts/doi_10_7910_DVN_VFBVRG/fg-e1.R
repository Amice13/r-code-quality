################################################################################
# Created By: Pietryka
# Creation Date:  2017-06-29
# Purpose: PLOT RELATIONSHIP BETWEEN OFF-THE-SHELF AND CORRECTED SCALES
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: Data/Derived/2012/4-Compare_New_and_Old.RData
# Output: Text/Figures/cormat_12.pdf, Text/Figures/cormat_12.tiff
#         Text/Figures/cormat_16.pdf, Text/Figures/cormat_16.tiff
# NOTES: Input data created in 'AP12-4-Compare_New_and_Old.R'
#
# Questions: mpietryka@fsu.edu
################################################################################


# 1.  LOAD PACKAGES ----------------
library(tidyverse) # MANY CONVENIENCE FUNCTIONS
library(stringr)   # STRING FUNCTIONS
library(scales)    # PRETTY SCALES IN PLOTS
library(survey)    # LOAD LIBRARY FOR WEIGHTED REGRESSION MODELS


# HOW TO HANDLE LONELY PSU (STRATUM 111 HAS ONLY ONE PSU)
## http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options(survey.lonely.psu = "adjust")

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




# 4.  CLEAN DATA   --------------------------------

# FUNCTION TO FORMAT CORRELATION FOR PLOT
format_cor <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }



# FUNCTION TO EXTRACT WEIGHTED CORRELATION
get_cor <- function(svy_design){
  v <- svyvar(~ z_all + z_ability, design = svy_design, na.rm = TRUE)
  varcov <- cov2cor(as.matrix(v))
  r <- varcov[1, 2]
  return(r)
}



# 5. MAIN PLOTTING DATA  --------------------------------
prep_data <- function(df, df_year){
  df  %>%
    # GET WEIGHTED CORREALTION
    mutate(cor_w = map_dbl(svy_design, get_cor))  %>%
    select(scale_name, group_var, data, cor, cor_w, no_good_items, no_dif)  %>%
    unnest()  %>%
    # INDICATE SCALE QUALITY
    mutate(quality = ifelse(no_good_items == TRUE, "Invalid", "Corrected"))  %>%
    mutate(quality = ifelse(no_dif == TRUE, "No DIF", quality))  %>%
    # REPLACE MISSING IF NO GOOD ITEMS
    mutate_at(vars(z_all, z_ability),
              funs(ifelse(no_good_items == FALSE, ., NA))
    )  %>%
    # MERGE WITH GROUP VARIABLE LABELS
    left_join(labdata_group, by = c("group_var" = "group_abrv"))  %>%
    left_join(labdata_scale, by = c("scale_name" = "scale_abrv"))  %>%
    # ADD YEAR VARIABLE
    mutate(year = df_year)
}

plot_df_12 <- prep_data(compare_nested_12, 2012L)
plot_df_16 <- prep_data(compare_nested_16, 2016L)

# 6. CORRELATION PLOTTING DATA  --------------------------------


prep_cor <- function(df){
  df  %>%
    select(z_all, z_ability, group_lab, scale_lab, quality, cor_w)  %>%
    group_by(group_lab, scale_lab, quality, cor_w)  %>%
    summarize(
      z_all_min = min(z_all, na.rm = TRUE),
      z_all_max = max(z_all, na.rm = TRUE),
      z_ability_min = min(z_ability, na.rm = TRUE),
      z_ability_max = max(z_ability, na.rm = TRUE)
    )  %>%
    mutate(cor_str = format_cor(cor_w))  %>%
    mutate(cor_str = ifelse(cor_str ==  "NA", "", cor_str))  %>%
    mutate(z_all = z_all_min)  %>%
    mutate(z_ability = z_ability_min)
}


cor_df_12 <- prep_cor(plot_df_12)
cor_df_16 <- prep_cor(plot_df_16)






# 7. CORELATION MATRIX  --------------------------------



cor_mat <- function(df, year){
  ggplot(data = df, aes(x = 0, y = 0, label = cor_str)) +
    # HIGHLIGHT BOXES WITH NO GOOD ITEMS
    geom_rect(data = filter(df, quality !=  "No DIF"),
              aes(fill = quality, color = quality),
              xmin = -Inf, xmax = Inf,
              ymin = -Inf, ymax = Inf) +
    # PLOT CORRELATION
    geom_text(
      data = filter(df, quality != "Invalid"),
      size = 4.5,
      fontface = "bold"
    ) +
    coord_fixed() +
    labs(title = year,
         x ="", y = "") +
    facet_grid(group_lab ~ scale_lab, scales = "fixed", switch = "y") +
    theme_light(base_size = 15) +
    theme(
      strip.text.x = element_text(angle = 90, vjust = 0),
      strip.text.y = element_text(angle = 180, hjust = 1),
      strip.text = element_text(face = "bold", colour = axis_col),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white", colour=NULL),
      plot.title =  element_text(size = rel(1.8),
                                 face = "bold",
                                 hjust = -0.4),
      plot.subtitle =  element_text(color = axis_col),
      plot.margin = unit(c(0, 10, 0, 0), "lines")
    ) +
    scale_fill_manual(values = c(corrected_col, invalid_col)) +
    theme(legend.position = "none")

}

plot_12 <- cor_mat(cor_df_12, 2012)
plot_16 <- cor_mat(cor_df_16, 2016)


width <- 11
height <- 8

graphics.off()
windows(width, height)
plot_12
windows(width, height)
plot_16








# 8. SAVE  --------------------------------
dev.off()
# FUNCTION TO ADD ANNOTATIONS TO MARGINS
ann_margin <- function(lab, x, y, length = 0.035){
  grid.text( lab ,
             x = x,
             y = y,
             just = "left",
             gp = gpar(fontsize=12, col = axis_col)
  )

  grid.lines(x = unit(c(x - .005, x - length), "npc"),
             y = unit(c(y, y), "npc"),
             gp = gpar(col = axis_col, lwd = 2)
  )
}


pdf("cormat_12.pdf", width, height)
  plot_12
  ann_margin(expression(paste("Scales w/ ", bold("DIF"))),
             x = 0.81,
             y = 0.60,
             length = 0.042)
  ann_margin(expression(paste("Could ", bold("NOT"), " be ", bold("CORRECTED"))),
             x = 0.79,
             y = 0.48,
             length = 0.035)
  ann_margin(expression(paste("Scales ", bold("WITHOUT DIF"))),
             x = 0.79,
             y = 0.30,
             length = 0.066)
dev.off()
tiff("cormat_12.tiff",
     width,
     height,
     res = 600,
     compression = "lzw",
     units="in")
plot_12
ann_margin(expression(paste("Scales w/ ", bold("DIF"))),
           x = 0.81,
           y = 0.60,
           length = 0.042)
ann_margin(expression(paste("Could ", bold("NOT"), " be ", bold("CORRECTED"))),
           x = 0.79,
           y = 0.48,
           length = 0.035)
ann_margin(expression(paste("Scales ", bold("WITHOUT DIF"))),
           x = 0.79,
           y = 0.30,
           length = 0.066)
dev.off()




ggsave(filename = "cormat_16.pdf",
       plot = plot_16,
       width = width,
       height = height)
ggsave(filename = "cormat_16.tiff",
       plot = plot_16,
       width = width,
       height = height)


