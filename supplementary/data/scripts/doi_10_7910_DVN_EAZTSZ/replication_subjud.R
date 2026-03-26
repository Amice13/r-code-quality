# This file contains the code to replicate the findings, figures, and appendix
# for Courts as Election Monitors: Assessing Judicial Influence on Election Quality

# The following packages are required 
library(tidyverse)
library(vdemdata)
library(mgcv)
library(gratia)

# Data Preparation

data <- vdem

## removing NAs from variables of interest

data <- data %>% filter(!is.na(v2xel_frefair) & !is.na(v2juhcind) & 
                          !is.na(v2juncind) & !is.na(v2juhccomp) & 
                          !is.na(v2jucomp) & !is.na(e_gdppc) & 
                          !is.na(v2elembaut) & v2x_polyarchy & 
                          !is.na(v2elembcap) & !is.na(v2x_jucon) & 
                          !is.na(v2xlg_legcon))

#Calculating the Generalized Propensity Scores (GPS) for both High and Low Courts

## High Courts

gps_high <- lm(v2juhcind ~ v2juhccomp  + log(e_gdppc) + v2elembaut + 
                 v2x_polyarchy + v2elembcap + v2x_jucon + v2xlg_legcon +
                 factor(country_name) + factor(year), data = data)

data$gps_high_mean <- predict(gps_high, type = "response")

data$gps_high_sd <- sqrt(summary(gps_high)$sigma^2)

data$hc_gps <- dnorm(data$v2juhcind, mean = data$gps_high_mean, 
                     sd = data$gps_high_sd)

## Low Courts

gps_low <- lm(v2juncind ~ v2jucomp + log(e_gdppc) + v2elembaut + 
                v2x_polyarchy + v2elembcap + v2x_jucon + v2xlg_legcon + 
                factor(country_name) + factor(year), data = data)

data$gps_low_mean <- predict(gps_low, type = "response")

data$gps_low_sd <- sqrt(summary(gps_low)$sigma^2)

data$lc_gps <- dnorm(data$v2juncind, mean = data$gps_low_mean, 
                     sd = data$gps_low_sd)

# Results (Dose Response Functions)
## H1: High Court Independence 

gam_high <- gam(v2xel_frefair ~ s(v2juhcind) + hc_gps + log(e_gdppc) + 
                  v2elembaut + v2elembcap + v2x_polyarchy + v2xlg_legcon + 
                  v2x_jucon + factor(country_name) + factor(year), data = data)

df_highcourt <- smooth_estimates(gam_high, smooth = "s(v2juhcind)")

hc_drf <- ggplot(df_highcourt, aes(x = v2juhcind, y = .estimate)) + 
  geom_line(color = "steelblue") + 
  geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se), alpha = 0.2, 
              fill = "steelblue") + 
  theme_minimal() + 
  labs(title = "Dose Response: High Court Independence", 
       x = "High Court Independence",
       y = "Predicted Election Quality") 
hc_drf 

## H2: Low Court Independence

gam_low <- gam(v2xel_frefair ~ s(v2juncind) + lc_gps + log(e_gdppc) +
                 v2elembaut + v2elembcap + v2x_polyarchy + v2xlg_legcon + 
                 v2x_jucon + factor(country_name) + factor(year), data = data)

df_lowcourt <- smooth_estimates(gam_low, smooth = "s(v2juncind)")

lc_drf <- ggplot(df_lowcourt, aes(x = v2juncind, y = .estimate)) + 
  geom_line(color = "steelblue") + 
  geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se), alpha = 0.2, 
              fill = "steelblue") + 
  theme_minimal() + 
  labs(title = "Dose Response: Low Court Independence", 
       x = "Low Court Independence",
       y = "Predicted Election Quality") 
lc_drf

## H3: Joint Effects

gam_model <- gam(v2xel_frefair ~ s(v2juhcind) + s(v2juncind) + hc_gps + 
                   lc_gps + log(e_gdppc) + v2elembaut + v2elembcap + 
                   v2x_polyarchy + v2xlg_legcon + v2x_jucon + 
                   factor(country_name) + factor(year), data = data)

jeplot <- vis.gam(gam_model, view = c("v2juhcind", "v2juncind"), 
                  plot.type = "contour",
                  color = "cm",
                  xlab = "High Court Independence",
                  ylab = "Low Court Independence",
                  main = "Joint Contour Plot")
jeplot

# Appendix 
## Collinearity 

hc_lc_coll <- ggplot(data, aes(x = v2juhcind, y = v2juncind)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  theme_minimal() + 
  labs(title = "Collinearity between High and Low Court Independence",
       xlab = "High Court Independence", 
       ylab = "Low Court Independence")
hc_lc_coll

## Common Support

data$hc_treat_group <- cut(data$v2juhcind,
                           breaks = quantile(data$v2juhcind,
                                             probs = seq(0, 1, 0.25), na.rm = TRUE),
                           include.lowest = TRUE,
                           labels = c("Low", "Mid-Low", "Mid-High", "High"))
data$lc_treat_group <- cut(data$v2juncind,
                           breaks = quantile(data$v2juncind, 
                                             probs = seq(0, 1, 0.25), na.rm = TRUE),
                           include.lowest = TRUE,
                           labels = c("Low", "Mid-Low", "Mid-High", "High"))

hc_comsup <- ggplot(data, aes(x = hc_gps, fill = hc_treat_group)) + 
  geom_density(alpha = 0.4) + 
  theme_minimal() + 
  labs(x = "High Court GPS", fill = "Treatment Groups")
hc_comsup

lc_comsup <- ggplot(data, aes(x = lc_gps, fill = lc_treat_group)) + 
  geom_density(alpha = 0.4) + 
  theme_minimal() + 
  labs(x = "Low Court GPS", fill = "Treatment Groups")
lc_comsup

## Distribution of High and Low Court GPS Values

hc_hist <- ggplot(df_highcourt, aes(x = v2juhcind, y = .estimate)) + 
  geom_line(color = "steelblue") + 
  geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se), alpha = 0.2,
              fill = "steelblue") + 
  theme_minimal() + 
  labs(title = "Dose Response: High Court Independence", 
       x = "High Court Independence",
       y = "Predicted Election Quality") +
  geom_histogram(data = data, aes(x = v2juhcind, y = -..density..), alpha = 0.2)
hc_hist

lc_hist <- ggplot(df_lowcourt, aes(x = v2juncind, y = .estimate)) + 
  geom_line(color = "steelblue") + 
  geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se), alpha = 0.2, fill = "steelblue") + 
  theme_minimal() + 
  labs(title = "Dose Response: Low Court Independence", 
       x = "Low Court Independence",
       y = "Predicted Election Quality") +
  geom_histogram(data = data, aes(x = v2juncind, y = -..density..), alpha = 0.2)
lc_hist

## Removing Tails of GPS Values and Re-estimating

### High Courts

data_mid_hc <-  data %>% filter(v2juhcind <= 2 & v2juhcind >= -2)

gps_high_mid <- lm(v2juhcind ~ v2juhccomp  + log(e_gdppc) + v2elembaut + 
                     v2x_polyarchy + v2elembcap + v2x_jucon + v2xlg_legcon + 
                     factor(country_name) + factor(year), data = data_mid_hc)

data_mid_hc$gps_high_mean <- predict(gps_high_mid, type = "response")

data_mid_hc$gps_high_sd <- sqrt(summary(gps_high_mid)$sigma^2)

data_mid_hc$mid_hc_gps <- dnorm(data_mid_hc$v2juhcind, 
                                mean = data_mid_hc$gps_high_mean,
                                sd = data_mid_hc$gps_high_sd)

gam_high_mid <- gam(v2xel_frefair ~ s(v2juhcind) + mid_hc_gps + log(e_gdppc) + 
                      v2elembaut + v2elembcap + v2x_polyarchy + v2xlg_legcon +
                      v2x_jucon + factor(country_name) + factor(year), 
                    data = data_mid_hc)

df_highcourt_mid <- smooth_estimates(gam_high_mid, smooth = "s(v2juhcind)")

hc_drf_mid <- ggplot(df_highcourt_mid, aes(x = v2juhcind, y = .estimate)) + 
  geom_line(color = "steelblue") + 
  geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se), alpha = 0.2, 
              fill = "steelblue") + 
  theme_minimal() + 
  labs(title = "Dose Response: High Court Independence", 
       x = "High Court Independence",
       y = "Predicted Election Quality") 
hc_drf_mid 
### Low Courts

data_mid_lc <-  data %>% filter(v2juncind <= 2 & v2juncind >= -2)

gps_low_mid <- lm(v2juncind ~ v2jucomp  + log(e_gdppc) + v2elembaut + 
                    v2x_polyarchy + v2elembcap + v2x_jucon + 
                    v2xlg_legcon + factor(country_name) + 
                    factor(year), data = data_mid_lc)

data_mid_lc$gps_high_mean <- predict(gps_low_mid, type = "response")

data_mid_lc$gps_high_sd <- sqrt(summary(gps_low_mid)$sigma^2)

data_mid_lc$mid_lc_gps <- dnorm(data_mid_lc$v2juncind, 
                                mean = data_mid_lc$gps_low_mean, 
                                sd = data_mid_lc$gps_low_sd)

gam_low_mid <- gam(v2xel_frefair ~ s(v2juncind) + mid_lc_gps + log(e_gdppc) + 
                     v2elembaut + v2elembcap + v2x_polyarchy + v2xlg_legcon + 
                     v2x_jucon + factor(country_name) + factor(year),
                   data = data_mid_lc)

df_lowcourt_mid <- smooth_estimates(gam_low_mid, smooth = "s(v2juncind)")

lc_drf_mid <- ggplot(df_lowcourt_mid, aes(x = v2juncind, y = .estimate)) + 
  geom_line(color = "steelblue") + 
  geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se), alpha = 0.2, 
              fill = "steelblue") + 
  theme_minimal() + 
  labs(title = "Dose Response: Low Court Independence", 
       x = "Low Court Independence",
       y = "Predicted Election Quality") 
lc_drf_mid 
