########### FIGURE 5 ############

### This code produces figure 5 in the main text
### To save time, I directly read in regression results which are saved under "results_twfe.rds" and "results_sa.rds"
### The code to produce these results is in the file "aggregate_analysis_TWFE.R" and "aggregate_analysis_SA.R" respectively

##################

## Clear environment

rm(list=ls())

## Packages

library(dplyr)
library(ggplot2)

## Load model results

results_twfe <- readRDS("results_twfe.rds")
results_sa <- readRDS("results_sa.rds")

## Clean names etc for the plot

main_results_out <- rbind(results_twfe, results_sa) %>%
  filter(dataset == "ABcombined_rural") %>% ## Rural respondents data (main specifications)
  filter(dependent_var %in% c("trust_pres", "new_mean_trust")) %>%
  mutate(model = rep(c("a) TWFE\n", "b) Sun & Abraham 2021\n"), each=6), 
         type = rep(c("\nMean political\ntrust", "\nTrust\npresident"), 6))

## Make and print plot

ggplot(main_results_out, aes(x = type, y = estimate, 
                             ymin = conf.low, ymax = conf.high, 
                             col = factor(spec, levels = 
                                            c("Baseline","+ Country trends",
                                              "+ Conley 50km")), 
                             shape = factor(spec, levels = 
                                              c("Baseline","+ Country trends",
                                                "+ Conley 50km")))) +
  facet_grid(~factor(model, 
                     levels = c("a) TWFE\n", "b) Sun & Abraham 2021\n")), 
             scales = "free_x") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width=0.5)) +
  scale_y_continuous(limits = c(-0.175, 0)) +
  theme_minimal() +
  scale_colour_manual(values = c("darkblue", "darkgrey", "black")) +
  ylab("ATT (0-3 scale)\n") +
  theme(legend.position="bottom", 
        legend.title=element_blank(), 
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"),
        strip.text = element_text(size = 12))

