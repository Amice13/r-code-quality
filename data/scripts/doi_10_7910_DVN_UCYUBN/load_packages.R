library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggside)
library(rjags)
load.module("dic")
load.module("glm")
load.module("msm")
library(runjags)
runjags.options(silent.jags = FALSE, silent.runjags = FALSE,
  predraw.plots = FALSE, method = "rjparallel")
library(ggmcmc)
##library(devtools)
##load_all("~/est/packages/ggmcmc")
library(gridExtra)
library(GGally)
library(ggrepel)

library(colorspace)

#source("theme.R")
#theme_set(theme_xfim())

library(stringr)
#library(devtools)
#load_all("~/est/packages/PolicyPortfolios")
library(PolicyPortfolios)
library(cowplot)
library(corrplot)
library(scales)
library(forcats)
library(tibble)
library(stringr)
library(sjPlot)
#library(ggcorrplot)

# Define also color palettes
# For environmental and social sectors
pal.sector <- c(rgb(0, 149, 64, max = 255), rgb(255, 0, 0, max = 255))
# For Implementation costs and Policy feedback
pal.dimension <- c("#E69F00", "#0072B2")

# Side, rule growth or rule decrease
pal.side <- c("#D55E00", "#56B4E9")

# Type, Supply and demand
pal.type <- c("#D55E00", "#56B4E9")


# simple kable
skable <- function(data) {
    knitr::kable(data, booktabs = TRUE, digits = 3) %>% 
    kable_styling(font_size = 8)
}
# Function to produce nice tables in both latex and html
mykbl <- function(tb, tc = NULL) {
  if (knitr::is_latex_output()) {
    knitr::kable(tb, format = "latex", digits = 2, caption = tc, longtable = TRUE, booktabs = TRUE) %>%
      kable_styling(font_size = 7, latex_options = c("striped"))
  } else {
    knitr::kable(tb, format = "html", digits = 2, caption = tc, booktabs = TRUE) %>%
      kable_styling(font_size = 7, position = "center", bootstrap_options = "striped", full_width = T)
  }
}

pal.5models <- ursa::cubehelix(5, light = 200)

pal.3models <- unname(palette.colors(palette = "Okabe-Ito")[c(4,7,6)])
pal.4models <- unname(palette.colors(palette = "Okabe-Ito")[c(2,4,7,3)])
pal.5models <- unname(palette.colors(palette = "Okabe-Ito")[c(1,2,4,7,3)])

# Reorder covariates
cov.rename.reorder <- function(x) {
  # Define covariates order
  cov.order <- c(
                 "Issue salience", "Electoral competition", "Institutional fragmentation",
                 "Ideology, average",
                 "Corporatism",
                 "Regional autonomy (shared rule)",
                 "Policy costs (unemployment)", "Policy costs (pensions)",
                 "Policy costs (dummy)",
                 "Administrative reforms",
                 "Size",
                 "Debt (log)",
                 "GDPpc (log)",
    "Liberalization * Christian democracy",
    "Liberalization * Share Left/Liberal",
    "Liberalization * Religious organization consultation",
    "Liberalization * Level of Permissiveness",
    "Liberalization * Political Constraints"
  )
  # Relabel
  x <- as.character(x)
#  x[x == "Shared rule"] <- "Regional autonomy (shared rule)"
#  x[x == "Ideological difference with next government, absolute"] <- "Ideological difference\nbetween governments"
#  x[x == "Administrative coordination reforms (n)"] <- "Administrative reforms"
#  x[x == "Managerial autonomy"] <- "Politico-administrative separation"
#  x[x == ""] <- ""
  x[x == "Liberalization * Liberty"] <- "Liberalization * Level of Permissiveness"
  x[x == "Liberty"] <- "Level of Permissiveness"
  x[x == "Liberty (sum)"] <- "Level of Permissiveness (sum)"
  x <- factor(x)
  x <- fct_relevel(x, rev(cov.order))
  return(x)
}

# Reorder covariates
cov.rename.reorder.standalone <- function(x) {
  # Define covariates order
  cov.order <- c(
                 "Liberalization",
                 "Share Left/Liberal",
                 "Religious organization consultation",
                 "Religious organization consultation & Opposition",
                 "Religious organization consultation * Opposition",
                 "Religious organization consultation & Opposition / Women's participation",
                 "Religious organization consultation * Opposition / Women's participation",
                 "Religious influence",
                 "Religious influence / Women's participation",
                 "Religious influence\n/ Women political participation index",
                 "Political constraints",
                 #
                 "Level of Permissiveness",
                 "Level of Permissiveness (sum)",
                 "GDPpc (log)",
                 "Religiosity",
                 "Civil Society participation",
                 "% Women in Parliament",
                 "Women political participation index",
                 "Democracy (Liberal, V-Dem)",
                 #
                 "Propensity score Liberalization",
    "Liberalization * Share Left/Liberal",
    "Liberalization * Religious organization consultation",
    "Liberalization * Religious organization consultation & Opposition",
    "Liberalization * Religious organization\nconsultation & Opposition",
    "Liberalization * Religious organization consultation & Opposition / Women's participation",
    "Liberalization * Religious organization\nconsultation & Opposition\n/ Women's participation",
    "Liberalization * Religious influence",
    "Liberalization * Religious influence / Women's participation",
    "Liberalization * Religious influence\n/ Women political participation index",
    "Liberalization * Political Constraints",
    "Liberalization * Women political participation index",
    "Liberalization * Level of Permissiveness"
  )
  # Relabel
  x <- as.character(x)
#  x[x == "Shared rule"] <- "Regional autonomy (shared rule)"
#  x[x == "Ideological difference with next government, absolute"] <- "Ideological difference\nbetween governments"
#  x[x == "Administrative coordination reforms (n)"] <- "Administrative reforms"
#  x[x == "Managerial autonomy"] <- "Politico-administrative separation"
#  x[x == ""] <- ""
  x[x == "Liberalization * Liberty"] <- "Liberalization * Level of Permissiveness"
  x[x == "Liberty"] <- "Level of Permissiveness"
  x[x == "Liberty (sum)"] <- "Level of Permissiveness (sum)"
  x <- factor(x)
  x <- fct_relevel(x, rev(cov.order))
  return(x)
}


# Calculate bins in histogram overimposed in marginal effects plot
calc_bin <- function(x, bins=bins) {
  mn <- min(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  bw <- (mx-mn)/bins
  z <- seq(mn, mx, by=bw)
  if (length(z)==1) { # no variation on the parameter, manual table
    return(data.frame(x=c(mn-1, mn, mn+1), width=rep(1, 3), count=c(0, length(x), 0)))
  } else {
    count <- as.vector(table(cut(x, breaks=z)))
    return(data.frame(x=z[-length(z)], width=bw, count=count))
  }
}


