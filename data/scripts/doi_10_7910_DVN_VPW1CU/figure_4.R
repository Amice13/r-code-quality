########### FIGURE 4 ############

### This code produces figure 4 in the main text

##################

## Clear environment

rm(list=ls())

## Load packages

library(ggplot2)
library(dplyr)

## Load data

ABcombined <- readRDS("ABcombined.rds")

## Clean data to make plot

coverage_plot <- ABcombined %>%
  select(urbrur, ea_code, inside_1999:inside_2014) %>%
  distinct(ea_code, .keep_all=T)

long_data <- coverage_plot %>%
  pivot_longer(cols = starts_with("inside_"), 
               names_to = "year", 
               names_prefix = "inside_", 
               values_to = "inside_coverage") %>%
  mutate(year = as.numeric(year))

coverage_plot_data <- long_data %>%
  group_by(urbrur, year) %>%
  summarise(prop_inside = mean(inside_coverage, na.rm = TRUE)*100) %>%
  mutate(title = "Enumeration areas located inside mobile coverage")

## Make and print plot

coverage_plot <- ggplot(data=coverage_plot_data, 
                        aes(x=year, y=prop_inside, col=urbrur)) +
  theme(panel.border = element_rect(colour = "black", fill = NA)) +
  geom_line(data=coverage_plot_data, aes(x=year, y=prop_inside), 
            linewidth=0.9, alpha=0.8) +
  geom_point(shape="triangle") +
  scale_colour_manual(values = c("darkred", "grey35")) +
  theme_minimal() +
  scale_x_continuous(limits = c(1999, 2015),
                     breaks = c(1999, 2002, 
                                2005, 2008, 
                                2011, 2014)) +
  scale_y_continuous(labels = c("0", "25", "50", "75", "100%")) +
  ylab("Enumeration areas\nin coverage\n") +
  theme(legend.title=element_blank(), 
        legend.position="bottom", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title = element_text(colour = "#000000"),
    axis.text = element_text(colour = "#000000"), 
    plot.title = element_text(colour = "#000000", face = "bold"),
    axis.line.x = element_line(linewidth = 0.5, 
                               linetype = "solid", 
                               colour = "black"),
    axis.line.y = element_line(linewidth = 0.5, 
                               linetype = "solid", 
                               colour = "black")) +
  annotate("text", x = 2014.35, y = 98, 
           label = "Urban", color = "grey35", hjust = 0, fontface = "bold") + 
  annotate("text", x = 2014.35, y = 88, 
           label = "Rural", color = "red4", hjust = 0, fontface = "bold")

coverage_plot

