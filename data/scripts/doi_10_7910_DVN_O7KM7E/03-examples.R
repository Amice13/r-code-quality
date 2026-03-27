##### Setup -----
## If you haven't installed the included "bapvar" package,
## you can do so via the following:
# devtools::install("bapvar")
## Load required packages
library(bapvar)       ## For BaP-VAR sampling and analysis functions
library(dplyr)        ## For data wrangling
library(BayesPostEst) ## For generating tables from JAGS output
## Source auxiliary functions
source("code/00-util.R") ## The functions are documented (roxygen-style) there
## Ensure needed subdirectories exist
prepare_directories()


##### Data prep -----
## Read in replication data
dat <- read.csv("data/citation-data.csv")


##### Smith v. California -----
## Sample posterior for Smith
case <- "1959 U.S. LEXIS 1885"
tmp  <- dat %>% filter(cited_case == case)
res  <- bapvar(tmp, lags = 1)
## Compare results to Table 3
courts <- c("SC", "AC", "DC")
vars   <- c(outer(courts, courts, paste, sep = "1_"))
## Note: This command also requires "runjags" package is installed for some
## reason. If you don't have it, there's a fallback option after
mcmcReg(
    res$samples,
    pars = vars
)
## If can't use above, summarize posterior samples and inspect
# summ <- summary(res$samples)
# summ$statistics[vars, ]
# summ$quantiles[vars, ]
## Generate dynamics
dynamics <- bapvar_dynamics(
    samples = res$samples[[1]],
    data = tmp,
    covariates = c("age", "distance"),
    p = 1,
    h = 5,
    verbose = TRUE,
    check_nth = 25000
)
## Compare results to Figure 2
plot(
    dynamics$irf,
    var_names = c("Supreme Court", "Apellate Courts", "District Courts"),
    line_color = rep("black", 3),
    ci_color = rep("#33333333", 3)
)


##### Watts v. United States -----
## Sample posterior for Watts
case <- "1969 U.S. LEXIS 1871"
tmp  <- dat %>% filter(cited_case == case)
res  <- bapvar(tmp, lags = 1)
## Compare results to Table 4
courts <- c("SC", "AC", "DC")
vars   <- c(outer(courts, courts, paste, sep = "1_"))
## Note: This command also requires "runjags" package is installed for some
## reason. If you don't have it, there's a fallback option after
mcmcReg(
    res$samples,
    pars = vars
)
## If can't use above, summarize posterior samples and inspect
# summ <- summary(res$samples)
# summ$statistics[vars, ]
# summ$quantiles[vars, ]
## Generate dynamics
dynamics <- bapvar_dynamics(
    samples = res$samples[[1]],
    data = tmp,
    covariates = c("age", "distance"),
    p = 1,
    h = 5,
    verbose = TRUE,
    check_nth = 25000
)
## Compare results to Figure 3
plot(
    dynamics$irf,
    var_names = c("Supreme Court", "Apellate Courts", "District Courts"),
    line_color = rep("black", 3),
    ci_color = rep("#33333333", 3)
)
