###############################################################################-
# Author: Pietryka
# Creation Date:  2021-07-21
# Purpose: create Figure 5 (qualtrics inform/enjoy plot)
# Contact: mpietryka@fsu.edu
###############################################################################-


# 1. LOAD DATA & PACKAGES  =======================================


library(tidyverse)
library(janitor)
library(glue)


# Load data
qt_df <- read_rds("Data/qt_df.rds")

# Load plot preferences
source("TR-plot-preferences.R")




# 2. clean qualtrics data for plot ===============

qt_plot_df <- qt_df  %>%
  pivot_longer(starts_with("eval"))  %>%
  mutate(value = as_factor(value)  %>% fct_expand("Far below average")  %>%
           fct_relevel("Far below average", "Somewhat below average", "Average",
                       "Somewhat above average", "Far above average")
  )  %>%
  group_by(project, name)  %>%
  count(value)  %>%
  complete(value, fill = list(n = 0))  %>%
  mutate(
    prop = n/sum(n),
    pct = round(prop*100, 0)  %>% paste0("%"))  %>%
  mutate(item = ifelse(name == "eval_1",
                       "'How much student' ~ bold('learned') ~ 'from the'",
                       "'How much student' ~ bold('enjoyed') ~ 'the'"
  ))   %>%
  mutate(project = recode(project, !!!labs_media)) %>%
  mutate(label = glue("{item} ~ bold('{project}')"))  %>%
  arrange(project, item)  %>%
  mutate(label = fct_inorder(label))

# 3. Make Plot ===============

the_n = qt_df  %>%
  distinct(id)  %>%
  drop_na(id)  %>%
  nrow()

qt_plot <- qt_plot_df  %>%
  ggplot(aes(x = value, y = prop))+
  geom_bar(aes(fill =  value), stat = "identity", color = NA, alpha = 0.7) +
  geom_text(aes(label = pct), hjust = -0.03, color = col_lines, size = 4.5) +
  geom_hline(yintercept = 0, color = col_lines, linewidth = 1.1) +
  coord_flip() +
  scale_fill_viridis_d(end = 0.8)+
  scale_color_viridis_d(end = 0.8)+
  facet_wrap( ~ label,
              ncol = 1,
              labeller = label_parsed,
  ) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, .1))
  ) +
  labs(
    title = NULL,
    caption = glue("Source: Media & Politics, Spring & Fall 2021, N = {the_n}")
  ) +
  # ggtitle("Compared to other college assignments you have completed that take similar amounts of time,\nhow would you rate this assignment in terms of...") +
  ylab(NULL) +
  xlab(NULL) +
  theme_paper(grid = "Xx") +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.spacing = unit(0.1, "lines")
  )

graphics.off()
windows(height = 6, width = 5.5)
qt_plot


# 5. DISPLAY VERSION NUMBERS FOR R & PACKAGES IN USE ===========

sessionInfo()
