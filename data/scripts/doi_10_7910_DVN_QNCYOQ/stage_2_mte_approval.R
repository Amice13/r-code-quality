###############################################################################
## SCRIPT ID:
## Blame-Shifting in Presidential Systems: Corrective Effect of Ministerial Terminations on Approval
## Figure 1: Marginal treatment effects of dismissing tainted ministers on presidential approval

## Original code: February, March, and August 2023
## R version 4.3.3 (2024-02-29) -- "Angel Food Cake"

## Author: Bastián González-Bustamante
## ORCID: 0000-0003-1510-6820
## Website: https://bgonzalezbustamante.com

## DPhil/PhD Project at the University of Oxford
## Critical Events and Ministerial Turnover in Latin American Presidential Democracies
## Public Opinion Quarterly (POQ) Revise and Resubmit Round

## Current positions of the author:
## Post-doctoral Researcher and Lecturer at Leiden University
## Lecturer at the Universidad Diego Portales

## Deposited code: April 2025
## The data set and replication code are deposited on:
## Public Opinion Quarterly (POQ) Harvard Dataverse
## R version 4.5.0 (2025-04-11) -- "How About a Twenty-Six"
###############################################################################

###############################################################################
## 1. Clean environment
###############################################################################

rm(list = ls())

###############################################################################
## 2. Installation notes (if needed)
###############################################################################

## It is relevant to use lpSolveAPI v5.5.2.0.17.12
## You can install from source as follows:
## install.packages("lpSolveAPI_5.5.2.0-17.12.tar.gz", repos = NULL, type = "source")

###############################################################################

###############################################################################
## 3. Load packages
###############################################################################

library(ivmte)
library(dplyr)
library(ggplot2)

###############################################################################
## 4. Import data
###############################################################################

transf_data <- read.csv("transf_data.csv")

###############################################################################
## 5. Transformations and lags
###############################################################################

## Create lagged variables
transf_data$approval_t1 <- dplyr::lag(transf_data$approval, 1)
transf_data$approval_t2 <- dplyr::lag(transf_data$approval, 2)

## Round decimals
transf_data$approval            <- round(transf_data$approval, 13)
transf_data$lagged_dism_tainted <- round(transf_data$lagged_dism_tainted, 13)
transf_data$lagged_nonpartisan  <- round(transf_data$lagged_nonpartisan, 13)

###############################################################################
## 6. MTE estiamtes
###############################################################################

## Dismissal of tainted minister
## Bounds on the target parameter: [-8.7731, -8.5041]
ivmte(
  data      = subset(transf_data),
  target    = "ate",
  m0        = ~ u + coalition + u * coalition + I(u^2) + I(u^2) * coalition,
  m1        = ~ u + coalition + u * coalition + I(u^2) + I(u^2) * coalition,
  ivlike    = approval ~ (dismissal_tainted + age + I(age^2) + nonpartisan) * coalition,
  propensity = as.factor(dismissal_tainted) ~ (age + I(age^2) + nonpartisan) * coalition,
  noisy     = TRUE
)

## Short-term effect
## Bounds on the target parameter: [9.1374, 9.3188]
ivmte(
  data       = subset(transf_data),
  target     = "ate",
  m0         = ~ u + coalition + u * coalition + I(u^2) + I(u^2) * coalition,
  m1         = ~ u + coalition + u * coalition + I(u^2) + I(u^2) * coalition,
  ivlike     = approval ~ (lagged_dism_tainted + lagged_age + I(lagged_age^2) + lagged_nonpartisan) * coalition,
  propensity = as.factor(lagged_dism_tainted) ~ (lagged_age + I(lagged_age^2) + lagged_nonpartisan) * coalition,
  noisy      = TRUE
)

## Medium-term effect
## Bounds on the target parameter: [8.2871, 8.3376]
ivmte(
  data       = subset(transf_data),
  target     = "ate",
  m0         = ~ u + coalition + u * coalition + I(u^2) + I(u^2) * coalition,
  m1         = ~ u + coalition + u * coalition + I(u^2) + I(u^2) * coalition,
  ivlike     = approval_t1 ~ (lagged_dism_tainted + lagged_age + I(lagged_age^2) + lagged_nonpartisan) * coalition,
  propensity = as.factor(lagged_dism_tainted) ~ (lagged_age + I(lagged_age^2) + lagged_nonpartisan) * coalition,
  noisy      = TRUE
)

## Long-run equilibrium
## Bounds on the target parameter: [-2.8286, -2.2501]
ivmte(
  data       = subset(transf_data),
  target     = "ate",
  m0         = ~ u + coalition + u * coalition + I(u^2) + I(u^2) * coalition,
  m1         = ~ u + coalition + u * coalition + I(u^2) + I(u^2) * coalition,
  ivlike     = approval_t2 ~ (lagged_dism_tainted + lagged_age + I(lagged_age^2) + lagged_nonpartisan) * coalition,
  propensity = as.factor(lagged_dism_tainted) ~ (lagged_age + I(lagged_age^2) + lagged_nonpartisan) * coalition,
  noisy      = TRUE
)

###############################################################################
## 7. Plot 
###############################################################################

## Plot data frame
df_plot <- data.frame(
  Time  = c("Dismissal of \nTainted Minister",
            "Short-Term \nEffect",
            "Medium-Term \nEffect",
            "Long-Run \nEquilibrium"),
  Order = c(1, 2, 3, 4),
  Lower = c(-8.7731, 9.1374, 8.2871, -2.8286),
  Upper = c(-8.5041, 9.3188, 8.3376, -2.2501)
)

## Compute mean for each category
df_plot$Mean <- (df_plot$Lower + df_plot$Upper) / 2

## Sort data frame by 'Order'
df_plot <- df_plot[order(df_plot$Order), ]

## Factor 'Time' to preserve order in plot
df_plot$Time <- factor(df_plot$Time, levels = df_plot$Time)

## PNG with 600 dpi
png(
  filename = "figure_1.png",
  width    = 1024 * 4,
  height   = 768 * 4,
  units    = "px",
  res      = 600
)

## Create the plot
ggplot(df_plot, aes(x = Time, y = Mean, group = 1)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.03, color = "black") +
  geom_point(shape = 23, color = "black", fill = "darkgray", size = 1.5) +
  ## Grey dotted line connecting mean points
  geom_line(color = "grey70", linetype = "dotted", size = 1) +
  ## Darker grey dotted horizontal line at y=0 with a different linetype
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed", size = 0.5) +
  theme_minimal(base_size = 12) +
  labs(
    x = "\nQuarters",
    y = "Marginal Treatment Effect on Approval"
  ) +
  ggtitle(NULL) +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin      = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )

dev.off()

