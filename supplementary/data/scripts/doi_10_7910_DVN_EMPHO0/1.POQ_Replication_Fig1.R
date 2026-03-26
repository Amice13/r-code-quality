
# packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(rio)
library(ggpubr)
library(marginaleffects)
library(xtable)
library(purrr)
library(patchwork)

# import data
data <- rio::import("POQ_Replication/lca_dat.csv")

#====================================================================================#
#
# Figures 1
#
#====================================================================================#

# list DVs and labels
vars <- c(
  "product_quality_num", "price_num", "profits_num", "demand_num",
  "complement_num", "inequality_num", "worker_wages_num", "hiring_num"
)

labs <- c(
  "Quality", "Price", "Profits", "Demand",
  "Complement", "Inequality", "Wages", "Hiring"
)

# get average predictions for each DV
fit <- function(dep_var, data) {
  formula <- as.formula(
    paste(
      dep_var, "~ age_group_lca_30_44 + age_group_lca_45_64 + age_group_lca_65over + gender_lca_woman +
      income_lca_Low + income_lca_High + education_lca_some_college + education_lca_university + 
      education_lca_postgrad + race_lca_Black + race_lca_Asian + race_lca_Hispanic + race_lca_Other +
      urban_lca_rural + high_skill_lca_professionals + info_lca_no_info + manipulation_pass"
    )
  )
  mod  <- lm(formula, data = data)
  pred <- avg_predictions(mod)
  return(pred)
}

# apply function and return df of preds
all_preds <- map2_dfr(
  vars, labs,
  ~ fit(.x, data) %>% 
    mutate(variable = .y)
)

# re-order for plotting
all_preds <- all_preds %>%
  mutate(
    variable = factor(
      variable,
      levels = c("Hiring","Wages","Inequality","Complement",
                 "Price","Quality","Demand","Profits")
    ),
    group = case_when(
      variable %in% c("Profits", "Demand") ~ "Firm Benefits",
      variable %in% c("Quality", "Price")  ~ "Consumer Benefits",
      TRUE ~ "Worker Benefits"
    ),
    group = factor(
      group, 
      levels = c("Firm Benefits","Consumer Benefits","Worker Benefits")
    ))

# split panels
make_panel <- function(df, title) {
  ggplot(df, aes(x = estimate, y = variable, xmin = conf.low, xmax = conf.high)) +
    # midpoint line
    geom_vline(xintercept = 0.5,
               color     = "grey50",
               linetype  = "dashed",
               linewidth = 0.4) +
    geom_pointrange(size = 0.1, color = "#66A3CC") +
    geom_point(size = 1, color = "#66A3CC") +
    scale_y_discrete(drop = TRUE) +         
    labs(title = title, x = "Predicted Agreement (0–1)", y = NULL) +
    theme_minimal(base_size = 6.5) +
    xlim(0,1) +
    theme(
      plot.title = element_text(face = "bold", size = 6.5, hjust = 0.5),
      axis.text = element_text(size = 6.5),
      axis.title = element_text(size = 6.5),
      panel.grid.major.y = element_blank()
    )
}

panels <- all_preds %>%
  split(.$group) %>%
  imap(~ make_panel(.x, title = .y))

# blank x-axis for top two panels
axis_blank_x <- theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  axis.ticks.x = element_blank()
)

p1 <- panels[["Firm Benefits"]] + axis_blank_x
p2 <- panels[["Consumer Benefits"]] + axis_blank_x
p3 <- panels[["Worker Benefits"]]

# stack in three rows with ggpubr
combined_plot <- ggpubr::ggarrange(
  p1, p2, p3,
  ncol = 1,
  nrow = 3,
  heights = c(1, 1, 2),
  align = "v"
)
ggsave(filename = "POQ_Replication/Figure1.png", width = 5.5, height = 2.75)
