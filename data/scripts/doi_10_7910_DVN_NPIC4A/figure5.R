library(ggplot2)
library(ggthemes)
library(dplyr)

# plot running/elected by election ----------------------------------------

simple_results_FV_year <- read.table("results_year_FV.txt")
simple_results_KV_year <- read.table("results_year_KV.txt")

simple_results_FV_year <- 
  simple_results_FV_year %>% 
  mutate(mod = ifelse(mod == "women", "Women", "Men"),
         type = "National elections")

simple_results_KV_year <- 
  simple_results_KV_year %>% 
  mutate(mod = ifelse(mod == "women", "Women", "Men"),
         type = "Local elections")

simple_results_FV_year <- 
  bind_rows(simple_results_FV_year, simple_results_KV_year)

plot_younger_year <- 
  ggplot(data = simple_results_FV_year,
         aes(y = est, 
             x = year, 
             ymin = est + qnorm(0.025) * se,
             ymax = est + qnorm(0.975) * se,
             alpha = mod)) + 
  facet_grid(mod ~ type, as.table=FALSE) +
  geom_point(size = 2) + 
  geom_errorbar(width = 0, size = 1) + 
  theme_light() +
  scale_x_continuous(name = "Difference in year of birth", 
                     breaks = 1:6) + 
  scale_y_continuous(name = "Younger sister relative to younger brother (%-point)") + 
  geom_hline(yintercept = 0, lty = 2, alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.3, 1), guide = 'none') +
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
print(plot_younger_year)