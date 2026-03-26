############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################
## Note that results may differ slightly due to sampling noise.

rm(list = ls())
library(rstan)
library(shinystan)
library(ggplot2)
library(parallel)
library(gridExtra)
library(haven)
library(dplyr)
options(mc.cores = detectCores())
set.seed(7268453)

inc_polar <- read_sas("../Data/HousePolarization.sas7bdat")
times     <- length(inc_polar$year)
tvec      <- seq(1,times,1)
inc_polar$incshar <- inc_polar$top1wageshare/10

## Set up data for stan
data_list <- list(T      = times - 1,
                  y_head = head(inc_polar$polar, times - 1),
                  y_tail = tail(inc_polar$polar, times - 1),
                  z      = tail(inc_polar$incshar, times - 1))

fit1           <- stan("Stan/msm_tvtp.stan", data = data_list, chains = 4, iter = 4000,  init_r = 0.5, 
                       control = list(adapt_delta = .999, max_treedepth = 15))
fit1_params    <- extract(fit1, pars = c("alpha", "phi1", "phi2", "gamma", "lambda"))
save(fit1, file = '../Output/msm_tvtp.RData')

quant <- function(para){
  q <- quantile(para, p = c(0.05, 0.95))
  t <- paste0('[', sprintf("%.2f", q[1]), ', ', sprintf("%.2f", q[2]), ']')
  return(t)
}
par(mfrow=c(1,2))
texfile  <- "../Tables/Table 5-1.tex"
cat("\\begin{table}\\centering \\small \\ssp \n ",file=texfile,append=FALSE)
cat("\\caption{MSM-TVTP analysis of party polarization in the U.S. House, 63rd-111th Congresses} \n",file=texfile,append=TRUE)
cat("\\begin{tabular}{lc} \\hline \\\\ \n",file=texfile,append=TRUE)
cat(" & Point estimates and 90\\% credible intervals \\\\ \n", file=texfile,append=TRUE)
cat(paste0("$\\alpha_1$ & ", round(mean(fit1_params$alpha[,1]), 2), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("         & ", quant(fit1_params$alpha[,1]), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("$\\alpha_2$ & ", round(mean(fit1_params$alpha[,2]), 2), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("         & ", quant(fit1_params$alpha[,2]), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("$\\rho_1$ & ", round(mean(fit1_params$phi1), 2), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("         & ", quant(fit1_params$phi1), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("$\\rho_2$ & ", round(mean(fit1_params$phi2), 2), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("         & ", quant(fit1_params$phi2), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("$\\lambda_1$ & ", round(mean(fit1_params$lambda[,1]), 2), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("          & ", quant(fit1_params$lambda[,1]), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("$\\lambda_2$ & ", round(mean(fit1_params$lambda[,2]), 2), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("          & ", quant(fit1_params$lambda[,2]), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("$\\gamma$ & ", round(mean(fit1_params$gamma), 2), "\\\\ \n"), file=texfile,append=TRUE)
cat(paste0("       & ", quant(fit1_params$gamma), "\\\\ \n"), file=texfile,append=TRUE)
cat("\\\\ \\bottomrule \n \\end{tabular}\n \\begin{tablenotes} \n \\item \n {\\em Notes}: \\ssp N = 50. Results obtained from Hamiltonian MCMC implemented in Stan, run with four chains, 2,000 samples per chain, discarding the first 2,000. The coeffcient estimates are the mean of the draws from the posterior distribution and the bounds on the credible intervals are the 5th and 95th percentiles of the posterior distribution. Diagnostics (available on request) indicate convergence. \n \\end{tablenotes} \n \\end{tabular} \n \\hspace{\\fill} \n \\end{table}\n \n \n",file=texfile,append=TRUE)

axis_line_color <- "black"
theme_set(theme_classic())
theme_update(
  axis.line.x  = element_line(size = 3,   color = axis_line_color),
  axis.line.y  = element_line(size = 0.5, color = axis_line_color),
  legend.title = element_text(face = "bold"),
  legend.text  = element_text(size = 18),
  plot.title   = element_text(size = 11),
  strip.text   = element_blank(),
  strip.background = element_blank()
)

polar     <- inc_polar$polar[-1]
posterior <- extract(fit1, pars = c("p_scur_1_givencur")) # get distribution of filtered probs
probs     <- c(0.025, 0.5, 0.975)

quants    <- function(posterior, param) t(apply(posterior[[param]], 2, quantile, probs))
fprobs    <- quants(posterior, "p_scur_1_givencur")
colnames(fprobs) <- c("lb95", "est", "ub95")
yearseq <- seq(1913,2009,2)
congseq <- seq(63,111,1)

pdf("../Figures/Figure 5-8.pdf")
df <- data.frame(congseq, est = fprobs[,"est"], polar)
ggplot(df, aes(x = congseq)) +
  geom_line(aes(y = est, color = 'Prob(polarizing state)', linetype = 'Prob(polarizing state)'), lwd=1.2) + 
  geom_line(aes(y = polar, color = 'Difference in party means', linetype = 'Difference in party means'), lwd = 0.9) +
  scale_y_continuous("Difference in party means & predicted state probability", 
                     breaks = seq(0,1,0.2)) +
  scale_color_manual("",
                     breaks = c("Prob(polarizing state)", "Difference in party means"),
                     values = c('black', 'black')) +
  scale_linetype_manual("",
                        values = c("Prob(polarizing state)"=1, "Difference in party means"=2)) +
  scale_x_continuous(breaks = c(seq(65,110,5)), 
                     labels = c("  65 \n 1917", "  70 \n 1927", "  75 \n 1937", "  80 \n 1947", "  85 \n 1957", "  90 \n 1967", "  95 \n 1977", "  100 \n 1987", "  105 \n 1997", "  110 \n 2007")) +
  xlab("\n Congress/Year") +
  theme(axis.line.x  = element_line(colour = 'black', size = 0.5),
        text         = element_text(size=14, color="black"),
        axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.y  = element_text(size=14, color="black"),
        axis.text.x  = element_text(size=13, color="black"),
        legend.position  = "bottom",
        legend.text      = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key       = element_blank(),
        legend.background = element_rect(fill = "white", colour = "black", size = 0.2),
        panel.background  = element_rect(fill = "white", colour = "black")) +
  guides(fill     = guide_legend(keywidth = 2, keyheight = 1),
         linetype = guide_legend(keywidth = 2, keyheight = 1, override.aes = list(lwd = c(1.2,0.9))),
         colour   = guide_legend(keywidth = 2, keyheight = 1))
dev.off()

## Check if coeffs are different from each other
parms         <- extract(fit1, pars = c("phi1", "phi2", "lambda")) 
phidiffquants <- quantile((parms$phi1 - parms$phi2),probs)
ldiffquants   <- quantile((parms$lambda[,1] - parms$lambda[,2]),probs)
phidiffquants
ldiffquants
