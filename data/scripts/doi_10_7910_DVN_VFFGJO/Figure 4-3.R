############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################

rm(list=ls())
library(cmdstanr)
library(Matrix)
library(spdep)
library(ggplot2)
library(dplyr) 
library(reshape2)
library(gridExtra)
source('Macros/rw_matrix.R')
source('Macros/bias_plot.R')

load("../Data/CoxKatz.RData")

P <- rw_matrix(
  dim   = length(unique(ck_data$congress)), 
  which = "penalty", 
  type  = "rw1"
)
stan_data <- list(
  N        = nrow(ck_data),
  C        = length(unique(ck_data$congress)),
  Pinverse = solve(P),
  cong     = ck_data$congress - 45,
  nVotes   = ck_data$nvotes,
  nWins    = ck_data$majps,
  lvRatio  = ck_data$lnmajvavg
)

##  CmdStanR code
# - runs more smoothly using CmdStanR and the latest version of Stan
# - can ignore warnings during warmup
# - but sometimes initial values seem to matter and we get a chain or two
#   that behaves poorly (look for a chain that's a _lot_ slower than the others). 
# - instead of figuring out what to set for inits (which could be laborious) 
#   you can just rerun it (the seed argument isn't set so it should use different inits)
mod <- cmdstan_model("Stan/bias_estimate.stan")
fit <- mod$sample(
  data            = stan_data, 
  chains          = 4, 
  parallel_chains = 4, 
  adapt_delta     = 0.99,
  iter_warmup     = 1000, 
  iter_sampling   = 1000
)

save(fit,file="../Output/cmdstan_fit.Rdata")

## Create bias plot
lambda_posterior <- fit$draws(c("lambda"),format="matrix")
rho_posterior    <- fit$draws(c("rho"),format="matrix")

probs  <- c(0.025, .25, 0.5, 0.75, 0.975)
quants <- function(posterior, param) t(apply(posterior[[param]], 2, quantile, probs))
lambda <- t(apply(lambda_posterior, 2, quantile, probs))
rho    <- t(apply(rho_posterior, 2, quantile, probs))
colnames(lambda) <- colnames(rho) <- c("lb95", "lb50", "est", "ub50", "ub95")
lambda <- data.frame(cbind(Congress = 46:106, lambda))
rho    <- data.frame(cbind(Congress = 46:106, rho))

df <- data.frame(Congress  = 46:106, rbind(lambda, rho), 
                 parameter = rep(c("lambda", "rho"), each = nrow(lambda)))
df$parameter <- factor(df$parameter, levels = c("rho", "lambda"))
param_labels <- list(bquote(rho), bquote(lambda))

my_theme <- theme(
  axis.line.x = element_line(size = 3, color = "#222222"),
  axis.line.y = element_line(size = 0.5, color = "#222222"),
  axis.title = element_text(face = "bold", size = 13),
  plot.title = element_text(face = "bold", size = 14),
  legend.position = "bottom", legend.text = element_text(size = 12)
)

graph1 <- ggplot(df, aes(x = Congress, ymin = lb95, ymax = ub95, y = est, fill = parameter)) + 
  geom_hline(yintercept = 0, color = "black", linetype = 3) +
  geom_ribbon(alpha = 0.25) + 
  geom_line() + 
  geom_point(aes(shape = parameter),size=2) + 
  scale_fill_grey(name = "",  start = 0.1, end = 0.5, labels = param_labels) +
  scale_color_grey(name = "", start = 0.1, end = 0.5, labels = param_labels) +
  scale_shape(name = "", labels = param_labels) +
  scale_x_continuous(breaks = c(46, seq(50,100,10), 106),  
                     labels = c("46 \n 1879   ", "50 \n 1887 ", "60 \n 1907  ", "70 \n 1927  ", "80 \n 1947  ", 
                                "90 \n 1967  ", "100 \n 1987 ", "106 \n 1999 ")) +
  ylab("Parameter value") + 
  my_theme

bias_posterior <- fit$draws("bias", format = "matrix")
graph2         <- bias_plot(bias_posterior, ck_data) 

graph1_title <- graph1 + labs(captions = '(a) Original "rolling average" results for bias') + theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
graph2_title <- graph2 + labs(captions = '(b) STAR results for bias') + theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
ggsave(grid.arrange(graph1_title, graph2_title, ncol=1), 
       file = '../Figures/Figure 4-3.pdf',
       height = 10, width = 8, units = 'in', scale = 0.99)

