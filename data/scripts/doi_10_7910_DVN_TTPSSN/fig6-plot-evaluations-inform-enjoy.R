###############################################################################-
# Author: Pietryka
# Creation Date:  2021-07-21
# Purpose: create Figure 6 (evaluation inform/enjoy plot)
# Contact: mpietryka@fsu.edu
###############################################################################-


# 1. LOAD DATA & PACKAGES  =======================================

library(tidyverse)
library(janitor)
library(glue)


# Load data
eval_df <- read_rds("Data/eval_df.rds")

# Load plot preferences
source("TR-plot-preferences.R")

# 2. CLEAN DATA WITH AGREE ITEMS  =====================

eval_plot_df <- eval_df  %>%
  mutate(caseid = row_number())  %>%
  select(class_name , caseid, enjoy, inform)  %>%
  drop_na()  %>%
  pivot_longer(c(enjoy, inform))  %>%
  mutate(value = as_factor(value))  %>%
  group_by(class_name ,  name)  %>%
  count(value)  %>%
  complete(value, fill = list(n = 0))  %>%
  mutate(
    prop = n/sum(n),
    pct = round(prop*100, 0)  %>% paste0("%"))  %>%
  mutate(item = ifelse(name == "enjoy",
                       "'Most' ~ bold('enjoyable')",
                       "'Most' ~ bold('informative')"
  )  %>% fct_rev())  %>%
  mutate(focal = (value == lab_assignment))  %>%
  mutate(pct = ifelse(class_name == "Research Methods" & value == "Writing",
                      "(NA)", pct))  %>%
  ungroup()  %>%
  arrange(class_name)  %>%
  mutate(class_name = fct_inorder(class_name))
  # mutate(class_name = fct_relevel(class_name,  "Media & Politics",
  #                                 "Social Influence", "Research Methods"))



eval_plot_df$class_name


# 3. PLOT INFORM/ENJOY ITEMS  =====================




# number of observations for this plot

the_n <- eval_df  %>%
  drop_na(enjoy, inform)  %>%
  group_by(course_code )  %>%
  count()  %>%
  mutate(course_lab = recode(course_code, !!!labs_course_code))   %>%
  arrange(course_lab)   %>%
  mutate(course_lab = fct_inorder(course_lab)) %>%
  glue_data("{course_lab}, N = {n}")   %>%
  glue_collapse(sep = "\n")

eval_plot <- eval_plot_df  %>%
  ggplot(aes(x = value, y = prop))+
  geom_bar(aes(fill =  focal), stat = "identity", color = NA) +
  geom_text(aes(label = pct, color = (pct == "(NA)")), hjust = -0.07, size = 4.5) +
  geom_hline(yintercept = 0, color = col_lines, linewidth = 1.1) +
  coord_flip() +
  # scale_fill_viridis_d(end = 0.8)+
  scale_fill_manual(values = colors_focal) +
  scale_color_manual(values = c(col_lines, colors_focal[[2]])) +
  facet_grid( class_name ~ item,
              labeller = labeller(item = label_parsed),
              switch = "y"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, .3))
  ) +
  labs(
    title = NULL,
    caption = glue("Source:\n{the_n}")
  ) +
  ylab(NULL) +
  xlab(NULL) +
  theme_paper(grid = "Yy") +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 90)
  )


graphics.off()
windows(height = 5.8, width = 5.5)
eval_plot





# 4. DISPLAY VERSION NUMBERS FOR R & PACKAGES IN USE ===========

sessionInfo()

