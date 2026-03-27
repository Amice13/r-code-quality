########### FIGURE 2 ############

### This code produces figure 2 in the main text
### This includes running relevant regression analyses and using them to create a coefficient plot

## Clear environment

rm(list=ls())

## Load packages

library(fixest)
library(dplyr)
library(broom)
library(ggplot2)
library(mosaic)
library(grid)
library(gridExtra)

## Load data

ABcombined <- readRDS("ABcombined.rds")

## Create new dataset with standardised outcomes for ease of presentation
ABcombined_urbrurplot <- ABcombined %>%
  mutate(new_mean_trust = as.numeric(scale(new_mean_trust)),
         trust_pres = as.numeric(scale(trust_pres)),
         trust_police = as.numeric(scale(trust_police)),
         trust_ec = as.numeric(scale(trust_ec)),
         trust_courts = as.numeric(scale(trust_courts)))

## Regressions to capture overall urban-rural gaps

urbrur1 <- feols(trust_pres ~ urbrur + age + gender + education | 
                   country_name + year, data=ABcombined_urbrurplot,
                 vcov = "iid")

urbrur2 <- feols(trust_police ~ urbrur + age + gender + education | 
                   country_name + year, data=ABcombined_urbrurplot,
                 vcov = "iid")

urbrur3 <- feols(trust_ec ~ urbrur + age + gender + education | 
                   country_name + year, data=ABcombined_urbrurplot,
                 vcov = "iid")

urbrur4 <- feols(trust_courts ~ urbrur + age + gender + education | 
                   country_name + year, data=ABcombined_urbrurplot,
                 vcov = "iid")

## Save data and make left-hand panel of the figure

urbrur_list <- list(urbrur1, urbrur2, urbrur3, urbrur4)

urbrur_df <- map_df(urbrur_list, broom::tidy, .id="model") %>%
  filter(term == "urbrurUrban") %>% 
  mutate(conf.low = estimate - 1.96*std.error, conf.high = estimate + 1.96*std.error,
         outcome = c("President", "Police", 
                     "Electoral\ncommission", "Courts"), 
         title = "a) Continent-wide\n")

urbrur_plot1 <- ggplot(urbrur_df, aes(y=factor(outcome,
                                               levels = c("Courts", "Electoral\ncommission",
                                                          "Police", "President")), 
                                      x=estimate)) +
  facet_wrap(~title) +
  geom_point(position = position_dodge(width=0.5)) +
  geom_vline(xintercept=0, linetype=2) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height=0),
                 position = position_dodge(width=0.5)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.title = element_blank(), 
        legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "black"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "black"),
        panel.spacing = unit(2, "lines")) +
  scale_x_continuous(limits = c(-0.2, 0), 
                     breaks = c(-0.2, -0.1, 0), 
                     labels = c("\n-20", "\n-10", "\n0")) +
  theme(
    axis.title = element_text(colour = "#000000"),
    axis.text = element_text(colour = "#000000"), 
    plot.title = element_text(colour = "#000000", face = "bold"),
    strip.text = element_text(size = 12))

## Re-run regression individually by country and create right-hand of the panel

country_results <- ABcombined %>%
  group_by(country_name) %>%
  nest() %>%
  mutate(model = map(data, ~ feols(new_mean_trust ~ urbrur + 
                                     + age + gender + education + as.factor(year),
                                   data = .x)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

# Extract coefficients related to the variables of interest
country_coefficients <- country_results %>%
  filter(term %in% c("urbrurUrban")) %>%
  dplyr::select(country_name, term, estimate, std.error) %>%
  mutate(outcome = "Mean trust", 
         spec = "Baseline", 
         title = "b) Country-by-country\n")

urbrur_plot2 <- ggplot(country_coefficients, 
                       aes(x = reorder(country_name, estimate), y = estimate)) +
  facet_wrap(~title) +
  geom_point() +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(
    ymin = estimate - 1.96 * std.error, 
    ymax = estimate + 1.96 * std.error), 
    width = 0.2) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "black"),
        axis.line.y = element_line(size = 0.5, 
                                   linetype = "solid", 
                                   colour = "black"),
        panel.spacing = unit(2, "lines")) +
  scale_y_continuous(limits = c(-0.6, 0.3), 
                     breaks = c(-0.6, -0.4, -0.2, 0, 0.2), 
                     labels = c("\n-60", "\n-40", "\n-20", "\n0", "\n20")) +
  coord_flip() +
  theme(
    axis.title = element_text(colour = "#000000"),
    axis.text = element_text(colour = "#000000"), 
    plot.title = element_text(colour = "#000000", face = "bold"),
    strip.text = element_text(size = 12))

## Print figure with both panels combined
grid.arrange(
  urbrur_plot1, 
  nullGrob(),
  urbrur_plot2, 
  nrow = 1, 
  widths = c(1, 0.1, 1),
  bottom = textGrob(
    "Percent (standard deviation)",
    gp = gpar(fontsize = 10, font = 1, colour = "#000000")
  )
)


