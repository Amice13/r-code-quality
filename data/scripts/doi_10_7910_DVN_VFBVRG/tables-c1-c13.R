################################################################################
# Created By: Pietryka
# Creation Date:  2017-03-15
# Purpose: Create tables C1-C13, summarizing ANOVA results
# R Version: R version 3.3.3 (2017-03-06)
# Data Input:
#             1. "Data/Derived/2012/AP12-3-DIF.RData"
#             2. "Data/Derived/2016/AP16-3-DIF.RData"
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

# 'dif_nested_12' OBJECT CREATED IN 'Analysis\AP12-3-DIF.R'
load("Data/Derived/2012/AP12-3-DIF.RData")

# 'dif_nested_16' OBJECT CREATED IN 'Analysis\AP16-3-DIF.R'
load("Data/Derived/2016/AP16-3-DIF.RData")



# 4. CLEAN DATA   --------------------------------

# FUNCTION TO EXTRACT, CLEAN DATA
extract_anovas <- function(df, the_year){
  df  %>%
    # EXTRACT ANOVAS
    select(scale_name, group_var, anova, no_good_items)  %>%
    unnest() %>%
    # REMOVE ROWS FOR RESIDUALS
    filter(term != "Residuals")  %>%
    # IDENTIFY GROUPS COMBINED IN ITEM
    mutate(split_groups = str_extract(item, "[0-9]+") )    %>%
    mutate(split_groups = gsub("(.{1})", "\\1 ", split_groups))   %>%
    separate(split_groups , paste0("value", 1:5), sep = " " )  %>%
    mutate_at(vars(starts_with("value")), as.numeric)  %>%
    select_if(function(col) (unique(col)  %>% length()) > 1)  %>%
    # REMOVE SPLIT IDs FROM ITEM NAME
    mutate(word1 = word(item, 1, sep = "_"))  %>%
    mutate(word2 = word(item, 2, sep = "_"))  %>%
    mutate(item = paste(word1, word2, sep = "_"))  %>%
    mutate(item = ifelse(scale_name == "wordsum_vars", paste0(item, "_crt"),
                         item))  %>%
    # ITEM ID
    group_by(scale_name)  %>%
    # Is statistically significant?
    mutate(ss = ifelse(bh_pval < .05, 1, 0) )  %>%
    # MERGE WITH GROUP VARIABLE LABELS
    left_join(labdata_group, by = c("group_var" = "group_abrv"))  %>%
    left_join(labdata_scale, by = c("scale_name" = "scale_abrv"))  %>%
    left_join(item_ids, by = c("scale_name" = "scale_abrv", "item"))  %>%
    # MERGE WITH VALUE LABELS
    left_join(group_labs_df,
              by = c("group_lab", "value1" = "anova_id"))  %>%
    left_join(group_labs_df,
              by = c("group_lab", "value2" = "anova_id"))  %>%
    left_join(group_labs_df,
              by = c("group_lab", "value3" = "anova_id"))  %>%
    left_join(group_labs_df,
              by = c("group_lab", "value4" = "anova_id"))  %>%
    unite(combined_values,
          value_lab.x,
          value_lab.y,
          value_lab.x.x,
          value_lab.y.y,
          sep = "; ")  %>%
    mutate(combined_values = str_replace_all(combined_values,
                                             "NA;| NA;| NA", "")) %>%
    # ADD YEAR VARIABLE
    mutate(year = the_year)
}

# EXTRACT, CLEAN DATA
extracted_12 <- extract_anovas(dif_nested_12, 2012)
extracted_16 <- extract_anovas(dif_nested_16, 2016)
extracted_anovas <- extracted_12  %>%
  rbind(extracted_16)  %>%
  group_by(scale_lab)  %>%
  nest() %>%
  arrange(scale_lab)



# 5. FORMAT DATA   --------------------------------


# FUNCTION TO FORMAT DATA
format_data <- function(df){
  df  %>%
    ungroup()  %>%
    filter(combined_values == "")  %>%
    select(Year = year,
           `Grouping Variable` = group_lab,
           Iteration = run,
           `Item #` = item_id,
           DF = df,
           `Sum of Squares` = sumsq,
           `Mean Square` = meansq,
           F = statistic,
           `BH p-value`= bh_pval
    )  %>%
    mutate_if(is.double, round, digits = 2)  %>%
    mutate_at(vars(Year, Iteration, DF), as.integer)  %>%
    arrange(Year, `Grouping Variable`, Iteration, `Item #`)
}

# FORMAT DATA
extracted_anovas <- extracted_anovas  %>%
  mutate(formatted_data = map(data, format_data))



# 6. CREATE X TABLE   --------------------------------




library(xtable)

# FUNCTION TO MAKE X TABLE
make_xtable <- function(df, scale){
  label <- scale  %>%
    str_replace(": ", "_")  %>%
    str_replace_all(" ", "_")  %>%
    str_replace_all("\\.", "")  %>%
    tolower()
  title <- paste("ANOVA results for", scale, "scale")

  x <- df   %>%
    xtable(caption = title,
           label = paste0("tab:anova_table_", label))
  return(x)
}


# MAKE X TABLE
extracted_anovas <- extracted_anovas  %>%
  mutate(xtable = map2(formatted_data, scale_lab, make_xtable))


# FUNCTION TO SAVE X TABLES
save_xtable <- function(xtab, scale){
  # FORMAT FILE NAME
  filename <- scale  %>%
    str_replace(": ", "_")  %>%
    str_replace_all(" ", "_")  %>%
    str_replace_all("\\.", "")  %>%
    tolower()

  # FORMAT ROWS
  rws <- seq(1, (nrow(xtab) - 1), by = 2)
  col <- rep("\\rowcolor[gray]{0.95}", length(rws))

  rws      <- c(0, rws)
  col  <- c(
    paste(
      "\\hline \n",
      "\\endhead \n",
      "\\hline \n",
      "\\multicolumn{8}{l@{}}{{\\scriptsize ",
      scale,
      " ANOVA results continued on next page}} \n",
      "\\endfoot \n",
      "\\endlastfoot \n",
      sep = ""
    ),
    col
  )

  row_format <- list(pos = as.list(rws), command = col)

  print(xtab,
        add.to.row = row_format,
        include.rownames = FALSE,
        hline.after = c(-1),
        tabular.environment = 'longtable',
        floating = FALSE,
        size = "\\fontsize{7pt}{3pt}\\selectfont",
        file = paste0("anova_table_", filename, ".tex"),
        caption.placement = "top")
}



# SAVE X TABLES
map2(extracted_anovas$xtable, extracted_anovas$scale_lab, save_xtable)



