library(ggplot2)
library(ggthemes)
library(dplyr)

# plot running/elected by election ----------------------------------------

simple_results_KV_year <- read.table("simple_results_KV_year.txt")

simple_results_KV_year$type <- as.factor(rep(rep(c("Running for office",
                                "Elected for office"),
                              each = 2), 6))
simple_results_KV_year$sex  <- rep(c("Women", "Men"), 12) 
simple_results_KV_year$type <- 
  factor(simple_results_KV_year$type, 
         levels = levels(simple_results_KV_year$type)[c(2,1)])

plot_younger_year <- 
  ggplot(data = simple_results_KV_year %>% filter(type == "Running for office"),
         aes(y = est, 
             x = year, 
             ymin = est + qnorm(0.025) * se,
             ymax = est + qnorm(0.975) * se,
             alpha = sex)) + 
  facet_grid(sex ~ ., as.table=FALSE) +
  geom_point(size = 2) + 
  geom_errorbar(width = 0, size = 1) + 
  theme_light() +
  scale_x_continuous(name = "Election year", 
                     breaks = unique(simple_results_KV_year$year)) + 
  scale_y_continuous(name = "Younger sister relative to younger brother (%-point)") + 
  geom_hline(yintercept = 0, lty = 2, alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.3, 1), guide = 'none') +
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle=45, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
print(plot_younger_year)