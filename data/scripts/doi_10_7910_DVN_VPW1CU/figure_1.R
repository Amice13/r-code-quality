########### FIGURE 1 ############

### This code produces figure 1 in the main text
### This is a descriptive plot, using ITU data, of mobile subscriptions over time by world region

##################

## Clear environment

rm(list=ls())

## Load packages

library(dplyr)
library(ggplot2)

## Load data

per100 <- read.csv("mobile-cellular-subscriptions-per-100-people.csv")
colnames(per100)[4] <- "out"

## Clean up region names for plot

per100 <- per100 %>%
  select(!Code) %>%
  filter(Entity %in% c("Sub-Saharan Africa (WB)", 
                       "Latin America and Caribbean (WB)", 
                       "Europe and Central Asia (WB)",
                       "India", "China",
                       "United States"),
         Year > 1994) %>%
  mutate(Entity = ifelse(Entity == "Sub-Saharan Africa (WB)", 
                         "e) Sub-Saharan Africa\n", 
                         ifelse(Entity == "Latin America and Caribbean (WB)", 
                                "d) Latin America and Caribbean\n", 
                                ifelse(Entity == "Europe and Central Asia (WB)", 
                                       "b) Europe and Central Asia\n", 
                                       ifelse(Entity == "India", 
                                              "c) India\n", 
                                              ifelse(Entity == "China", 
                                                     "a) China\n", 
                                                     ifelse(Entity == "United States", 
                                                            "f) United States\n", Entity)))))))

## Make and print plot

itu_plot <- ggplot(data=per100, aes(x=Year, y=out)) +
  facet_wrap(~Entity, nrow=2, scales="free_x") +
  geom_line(col="black", alpha=0.7) +
  geom_point(alpha=0.5, size=0.65, col="darkred") +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = 
          element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = 
          element_line(size = 0.5, linetype = "solid", colour = "black"),
        panel.spacing = unit(2, "lines")) +
  labs(y = "Subscriptions per 100 people\n", 
       x = "\nYear") +
  scale_x_continuous(breaks = c(1995, 2020)) +
  theme(
    axis.title.x = element_blank(),
    axis.title = element_text(colour = "#000000"),
    axis.text = element_text(colour = "#000000"), 
    plot.title = element_text(colour = "#000000", face = "bold"),
    strip.text = element_text(size = 10))

itu_plot
