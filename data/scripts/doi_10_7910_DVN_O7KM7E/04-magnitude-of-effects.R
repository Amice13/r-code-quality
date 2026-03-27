##### Setup -----
## If you haven't installed the included "bapvar" package,
## you can do so via the following:
# devtools::install("bapvar")
## Load required packages
library(bapvar) ## For BaP-VAR sampling and analysis functions
library(dplyr)  ## For data wrangling
library(tidyr)  ## For data wrangling
## Source auxiliary functions
source("code/00-util.R") ## The functions are documented (roxygen-style) there
## Ensure needed subdirectories exist
prepare_directories()


##### Data prep -----
## Read in raw data
citation_data <- read.csv("data/citation-data.csv")
## Read in stored results
coef_dists <- read.csv("data/coef-summaries.csv")
dynamics <- read.csv("data/dynamic-summaries.csv")
## Get the standard deviation of all equations for all precedents
sd_data <- citation_data %>%
    select(cited_case, DCcites, ACcites, SCcites) %>%
    group_by(cited_case) %>%
    summarise_at(vars(DCcites:SCcites), sd)
## Summarise the means, 2.5% quantiles, and 97.5% quantiles for all lag coefs
coef_means_summary <- coef_dists %>%
    select(cited_case, var, mean) %>%
    group_by(cited_case) %>%
    pivot_wider(names_from = var, values_from = mean) %>%
    merge(sd_data, by.x = "cited_case", by.y = "cited_case", all.x = TRUE) %>%
    mutate_at(vars(ends_with("_SC")), "/", .[["SCcites"]]) %>%
    mutate_at(vars(ends_with("_AC")), "/", .[["ACcites"]]) %>%
    mutate_at(vars(ends_with("_DC")), "/", .[["DCcites"]]) %>%
    select(-SCcites, -ACcites, -DCcites)
coef_lows_summary <- coef_dists %>%
    select(cited_case, var, low) %>%
    group_by(cited_case) %>%
    pivot_wider(names_from = var, values_from = low) %>%
    merge(sd_data, by.x = "cited_case", by.y = "cited_case", all.x = TRUE) %>%
    mutate_at(vars(ends_with("_SC")), "/", .[["SCcites"]]) %>%
    mutate_at(vars(ends_with("_AC")), "/", .[["ACcites"]]) %>%
    mutate_at(vars(ends_with("_DC")), "/", .[["DCcites"]]) %>%
    select(-SCcites, -ACcites, -DCcites)
coef_highs_summary <- coef_dists %>%
    select(cited_case, var, high) %>%
    group_by(cited_case) %>%
    pivot_wider(names_from = var, values_from = high) %>%
    merge(sd_data, by.x = "cited_case", by.y = "cited_case", all.x = TRUE) %>%
    mutate_at(vars(ends_with("_SC")), "/", .[["SCcites"]]) %>%
    mutate_at(vars(ends_with("_AC")), "/", .[["ACcites"]]) %>%
    mutate_at(vars(ends_with("_DC")), "/", .[["DCcites"]]) %>%
    select(-SCcites, -ACcites, -DCcites)


##### Reproduce Figures 4 and 5 -----
## Colors and point types to distinguish coefficients/courts
plot_colors <- rep("black", 3)
point_types <- 15:17
## Get quantiles of coefficient means
qs <- apply(coef_means_summary[ , -1], 2, quantile, probs = c(0.25, 0.5, 0.75))
## Variables to help order and label things in the plot
sc <- c("SC1_SC", "AC1_SC", "DC1_SC")
ac <- c("SC1_AC", "AC1_AC", "DC1_AC")
dc <- c("SC1_DC", "AC1_DC", "DC1_DC")
coef_order <- c(sc, ac, dc)
eq_names <- c("SC Cites", "AC Cites", "DC Cites")
x <- 1:9
xticks <- c(2, 5, 8)
xticklabs <- paste(eq_names, "Equation")
## Generate Figure 4
opar <- par(mar = c(3, 3, 1, 1) + 0.1)
trimplot(NULL, ylim = range(qs), xlim = c(1, 9))
close_axis(1, "", ticks_at = xticks, tick_labels = xticklabs)
close_axis(2, "Lag Coefficient / DV Standard Deviation")
abline(h = 0, lty = 2, col = "#00000099")
points(x, qs[2, coef_order], pch = point_types, col = plot_colors, cex = 1.5)
whisker(x, qs[1, coef_order], qs[3, coef_order], col = plot_colors)
legend("top", bty = "n", pch = point_types, col = plot_colors, lty = 1,
       cex = 1.25, legend = paste(eq_names, "Lag Coefficient"))
par(opar)
## Get mean of coefficient quantiles
qs1 <- apply(coef_means_summary[ , -1], 2, mean)
qs2 <- apply(coef_lows_summary[ , -1],  2, mean)
qs3 <- apply(coef_highs_summary[ , -1], 2, mean)
## Generate Figure 5
opar <- par(mar = c(3, 3, 1, 1) + 0.1)
trimplot(NULL, ylim = range(c(qs2, qs3)) + c(-0.01, 0.01), xlim = c(1, 9))
close_axis(1, "", ticks_at = xticks, tick_labels = xticklabs)
close_axis(2, "Lag Coefficient / DV Standard Deviation")
abline(h = 0, lty = 2, col = "#00000099")
points(x, qs1[coef_order], pch = point_types, col = plot_colors, cex = 1.5)
whisker(x, qs2[coef_order], qs3[coef_order], col = plot_colors)
legend("top", bty = "n", pch = point_types, col = plot_colors, lty = 1,
       cex = 1.25, legend = paste(eq_names, "Lag Coefficient"))
par(opar)


##### Reproduce Figure 6 and 7 -----
## Get quantiles of FEVD means
fevds <- dynamics %>% filter(dynamic == "FEVD" & step == 2)
fevd_means <- fevds %>% filter(quantity == "mean") %>% select(SC_to_SC:DC_to_DC)
qs <- apply(fevd_means, 2, quantile, probs = c(0.25, 0.5, 0.75))
coef_order <- colnames(qs)
## Generate Figure 6
opar <- par(mar = c(3, 3, 1, 1) + 0.1)
trimplot(NULL, ylim = c(0, 1), xlim = c(1, 9))
close_axis(1, "", ticks_at = xticks, tick_labels = xticklabs)
close_axis(2, "Forecast Error Variance Decomposition")
points(x, qs[2, coef_order], pch = point_types, col = plot_colors, cex = 1.5)
whisker(x, qs[1, coef_order], qs[3, coef_order], col = plot_colors)
legend(2, 1, bty = "n", pch = point_types, col = plot_colors, lty = 1,
       cex = 1.25, legend = eq_names, title = "FEV Due To:")
par(opar)
## Get means of FEVD quantiles
qs <- fevds %>% group_by(quantity) %>% summarise_at(vars(SC_to_SC:DC_to_DC), mean)
qs <- as.matrix(qs[ , -1], dimnames = c(qs$quant, colnames(qs)[-1]))
## Generate Figure 7
opar <- par(mar = c(3, 3, 1, 1) + 0.1)
trimplot(NULL, ylim = c(0, 1), xlim = c(1, 9))
close_axis(1, "", ticks_at = xticks, tick_labels = xticklabs)
close_axis(2, "Forecast Error Variance Decomposition")
points(x, unlist(qs[2, coef_order]), pch = point_types, col = plot_colors,
       cex = 1.5)
whisker(x, unlist(qs[1, coef_order]), qs[3, coef_order], col = plot_colors)
legend(2, 1, bty = "n", pch = point_types, col = plot_colors, lty = 1,
       cex = 1.25, legend = eq_names, title = "FEV Due To:")
par(opar)
