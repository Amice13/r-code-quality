############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################

rm(list = ls())
library(foreign)
library(plyr)
library(rstan)
source('Macros/plot_rhos.R')

## Prepare data for Stan
data   <- read.dta("../Data/JacksonKollman.dta")
pid    <- data$pid
pid1   <- data$pid1
pid2   <- data$pid2
util1  <- data$util1
util11 <- data$util11
t <- data$year
T <- length(unique(t)) 

## Make indicators for groups (south/black combinations)
Nwh <- with(data, south == 0 & black == 0)
Nbl <- with(data, south == 0 & black == 1)
Swh <- with(data, south == 1 & black == 0)
Sbl <- with(data, south == 1 & black == 1)
group_names <- c("Nwh", "Nbl", "Swh", "Sbl")
J <- length(group_names)

## Make some empty TxJ matrices
mat_names <- c("party", "party_L1", "party_L2", "util", "util_L1")
for (name in mat_names) {
  assign(name, matrix(NA, nrow = T, ncol = J, dimnames = list(NULL, group_names)))
}

for (j in 1:J) {
  grp <- get(group_names[j])
  party[, j]    <- pid[grp]
  party_L1[, j] <- pid1[grp]
  party_L2[, j] <- pid2[grp]
  util[, j]     <- util1[grp]
  util_L1[, j]  <- util11[grp]
}

by_year <- ddply(data, "year", summarise, 
                 retro1  = mean(retro1),
                 retro11 = mean(retro11))
econ    <- by_year$retro1
econ_L1 <- by_year$retro11

## Data to pass to Stan
stan_data <- list(
  party    = party,
  party_L1 = party_L1,
  party_L2 = party_L2,
  util     = util,
  util_L1  = util_L1,
  econ     = econ,
  econ_L1  = econ_L1,
  J = J, 
  T = T 
)

rm(list = setdiff(ls(), c("stan_data", "plot_rhos")))
for (fname in c("jk_basic_tighter_priors_rho_logit_ar1", "jk_rho_unc_gp")){
  stan_file <- file.path('Stan', paste0(fname, ".stan"))
  save_file <- file.path('../Output', paste0(fname, ".RData"))
  result    <- stan(file = stan_file, data = stan_data, iter = 3000, chains = 1)
  assign(fname, result)
  save(list = fname, file = save_file)
}

pdf('../Figures/Figure 5-5.pdf')
plot_rhos(jk_basic_tighter_priors_rho_logit_ar1, unconstrained=F)
dev.off()

pdf('../Figures/Figure 5-6.pdf')
plot_rhos(jk_rho_unc_gp, unconstrained=T)
dev.off()


