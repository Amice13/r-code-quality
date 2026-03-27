########### FIGURE 6 ############

### This code produces figure 6 in the main text and runs the underlying regression analyses
### Those with spatial SEs use restricted access data, as discussed in the readme. 
### If you don't have access to this data, you can read instead the results to make the plot from "event_study3.rds"
### The code for event_study1 and event_study2 do not use restricted data, so can be run universally

##################

## Clear environment

rm(list=ls())

## Packages

library(dplyr)
library(fixest)
library(ggplot2)
library(stringr)
library(purrr)

##### PART 1: Running regressions

## Load data

ABcombined_rural <- readRDS("ABcombined.rds") %>%
  filter(urbrur == "Rural")

## Run individual models (same as in the wider loop previously, saving coefficients for relative time periods)

event_study1 <- feols(new_mean_trust ~ sunab(first_inside, time_to_inside, bin.rel = "bin::2") +
                        news_radio + news_tv + news_paper + election_year + nightlights_present +
                        cash_income + food + water + medical + age + gender + education | 
                        country_name + year, 
                      ABcombined_rural,
                      vcov = ~ea_code)

event_study2 <- feols(new_mean_trust ~ sunab(first_inside, time_to_inside, bin.rel = "bin::2") +
                        news_radio + news_tv + news_paper + election_year + nightlights_present +
                        cash_income + food + water + medical + age + gender + education | 
                        country_name[year] + year, 
                      ABcombined_rural,
                      vcov = ~ea_code)

### NOTE: event_study3 below requires restricted access data
### If you do not have access to the data, you can read in the file underneath
#event_study3 <- feols(new_mean_trust ~ sunab(first_inside, time_to_inside, bin.rel = "bin::2") +
#news_radio + news_tv + news_paper + election_year + nightlights_present +
#cash_income + food + water + medical + age + gender + education | 
#country_name[year] + year, 
#ABcombined_rural,
#conley(50, distance="spherical")) 

#saveRDS(event_study3, file = "event_study3.rds")

### Code to load event_study3 back in if do not have access to sensitive data

event_study3 <- readRDS("event_study3.rds")

## Combine results in a cleaned dataframe

event_study <- list(event_study1, event_study2, event_study3)

dynamic_results <- map_df(event_study, broom::tidy, .id="model") %>%
  filter(str_detect(term, "time_to_inside")) %>%
  mutate(spec = ifelse(model == "1", "Baseline",
                       ifelse(model == "2", "+ Country trends",
                              "+ Conley 50km")), 
         conf.low = estimate - 1.96*std.error, 
         conf.high = estimate + 1.96*std.error,
         outcome = "Mean political trust",
         term_numeric = as.numeric(sub("time_to_inside::", "", term)))


#### PART 2: Make plot

ggplot(dynamic_results, aes(x = term_numeric, y = estimate, 
                            ymin = conf.low, ymax = conf.high, 
                            col = factor(spec, levels = 
                                           c("Baseline","+ Country trends",
                                             "+ Conley 50km")), 
                            shape = factor(spec, levels = 
                                             c("Baseline","+ Country trends",
                                               "+ Conley 50km")))) +
  geom_pointrange(position = position_dodge(width = 0.7)) +
  geom_hline(yintercept=0) +
  geom_point(position = position_dodge(width=0.7)) +
  theme_minimal() +
  scale_colour_manual(values = c("darkblue", "darkgrey", "black")) +
  ylab("Estimate and 95%\n confidence interval \n") +
  xlab("\nTime to/from entering coverage (two year bins)") +
  scale_x_continuous(breaks = seq(-14, 14, 4)) +
  theme(legend.position="bottom", 
        legend.title=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_vline(xintercept=-1, lty="dashed") +
  geom_hline(yintercept=0, linetype="dashed", col="#000000") +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))
