library(ggplot2)
library(ggthemes)
library(dplyr)

# plot ever running/elected -------------------------------------------

# simple_results_KV <- read.table("dst_res/simple_results_KV.txt")
predicted_KV <- read.table("predicted_means_KV.txt")

predicted_KV <- 
  predicted_KV %>% 
  mutate(pos = rep(c(2, 1), 4)) 


plot_younger <- 
  ggplot(data = predicted_KV %>% filter(outcome == "Ever run for office"),
         aes(x = pos, 
             y = mean, 
             ymin = mean + qnorm(0.025) * se,
             ymax = mean + qnorm(0.975) * se,
             alpha = sibling)) + 
  facet_grid(. ~ target, scales = "free_x", as.table=FALSE) +
  geom_point(size = 2) +
    geom_errorbar(width = 0, size = .5, colour= "black") +  
  theme_light() +
  scale_x_continuous(name = "", limits = c(0.5,2.5), labels = NULL, breaks = NULL) + 
  scale_y_continuous(name = "Probability of ever running (%)") + 
  scale_alpha_discrete(range = c(0.3, 1), "Younger sibling") +
  theme(axis.ticks.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
print(plot_younger)
