###############################################################################-
# Author: Pietryka
# Creation Date:  2021-07-21
# Purpose: create Figure 7 (evaluation agreement plot)
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

# number of observations for this plot
the_n <- eval_df  %>%
  drop_na(notice)  %>%
  nrow()


# create plotting data
agree_plot_df <- eval_df  %>%
  mutate(caseid = row_number())  %>%
  select(caseid, notice)  %>%
  separate(notice, into = c("choice1", "choice2"), sep = ",", fill = "right")  %>%
  pivot_longer(-caseid)  %>%
  drop_na()  %>%
  group_by(value)  %>%
  count()  %>%
  mutate(
    prop = n/the_n,
    pct = round(prop*100, 0)  %>% paste0("%")
  )


# 3. PLOT AGREE ITEMS  =====================




agree_plot <- agree_plot_df  %>%
  ggplot(aes(x = str_wrap(value, width = 46)  %>% fct_reorder(prop), y = prop)) +
  geom_bar(aes(fill =  value), stat = "identity", color = NA) +
  geom_text(aes(label = pct), hjust = -0.03, color = col_lines, size = 4.5) +
  geom_hline(yintercept = 0, color = col_lines, linewidth = 1.1) +
  coord_flip() +
  scale_fill_viridis_d(end = 0.8) +
  scale_color_viridis_d(end = 0.8) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, .20))
  ) +
  labs(
    title = "Percentage of respondents who agree with each statment",
    caption = glue("Source: Research Methods, Spring 2021, N = {the_n}",
                   "
                   Note: Items are not mutually exclusive"
    )
  ) +
  ylab(NULL) +
  xlab(NULL) +
  theme_paper(grid = "Xx", base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 1),
    plot.title.position = "plot"
  )

graphics.off()
windows(height = 2.8, width = 5.5)
agree_plot



# 4. DISPLAY VERSION NUMBERS FOR R & PACKAGES IN USE ===========

sessionInfo()
