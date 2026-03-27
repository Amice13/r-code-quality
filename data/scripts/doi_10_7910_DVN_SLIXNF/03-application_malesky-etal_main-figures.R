

##
## Generate figures and tables for the main text
##



## load packages
require(tidyverse)
require(DIDdesign)
require(knitr)
require(kableExtra)

## load data
data(malesky2014)
malesky2014 <- drop_na(malesky2014, lnarea, lnpopden, city)


## load results
check_out <- readRDS("../results/application/01-check_main_output.rds")
est_out   <- readRDS("../results/application/02-est_main_output.rds")


## parallel trends plot
dat_trend  <- map(check_out$out, ~.x$plot[[1]]$dat_plot)

##
## define function to plot
##

plot_trend <- function(dat, ...) {
  plot(1, 1, type = "n", xaxt = "n", xlab = "Year", ylab = "",
        cex.axis = 1.3, cex.lab = 1.3, xlim = c(0.9, 3.1),
        cex.main = 1.2, las = 1, ...)
  axis(side = 1, at = 1:3, labels = c("2006", "2008", "2010"), cex.axis = 1.3)
  polygon(x = c(2.5, 3.5, 3.5, 2.5),
          y = c(0, 0, 1, 1), col = adjustcolor("gray", 0.3),
          border = adjustcolor("gray", 0.6), lwd = 1.3, lty = 3)

  points(1:3, dat$outcome_mean[dat$group == "Control"], type = "o", cex = 1.5,
         col = "gray30", lwd = 2, pch = 21, bg = 'white', lty = 2)
  points(1:3, dat$outcome_mean[dat$group == "Treated"], type = "o", cex = 1.5,
         col = "black", lwd = 2, pch = 19)
}


##
## hard-coding variable names
##
cairo_pdf(filename = "../results/figures/figure3_malesky-parallel_trends_final.pdf", 
          height = 3, width = 9, bg = 'transparent')
par(mfrow = c(1,3), mar = c(4, 2, 2, 2), oma = c(0, 3, 0, 0), pty = 's')
plot_trend(dat_trend[['pro4']], ylim = c(0.1, 0.4), main = "Education and Cultural Program")
  legend("topleft", legend = c("Treatment", "Control"), col = c("black", "gray30"),
         lty = c(1, 2), pch = c(19, 21), cex = 1.3, bty = "n", bg = 'white')
plot_trend(dat_trend[['tapwater']], ylim = c(0.03, 0.35), main = "Tap Water")
plot_trend(dat_trend[['agrext']], ylim = c(0.04, 0.09), main = "Agricultural Center")
mtext(side = 2, at = 0.5, "Mean of Outcomes", outer = TRUE, line = 1, cex = 0.9)
dev.off()


##
## table
##
check_out$summary %>%
  mutate(var_name = case_when(
    var == "pro4" ~ "Education and Cultural Program",
    var == "tapwater" ~ "Tap Water",
    var == "agrext" ~ "Agricultural Center"
  )) %>%
  select(var_name, Estimate = estimate, `Std. Error` = std.error, `p-value` = p_value,
         LB = EqCI95_LB, UB = EqCI95_UB) %>%
  arrange(desc(`p-value`)) %>%
  kable(., format = 'latex', booktabs = TRUE, digits = 3) %>% 
  kableExtra::save_kable("../results/tables/table2.tex")


##
## effect plots
##
dat_effect <- est_out$summary %>%
  mutate(CI90_UB = estimate + qnorm(0.95) * std.error,
         CI90_LB = estimate - qnorm(0.95) * std.error) %>%
  filter(var == "tapwater" | var == "pro4")


tmp <- list(
  dat_effect %>% filter(var == "pro4" & estimator != "sDID") %>%
          arrange(estimator),
  dat_effect %>% filter(var == "tapwater" & estimator != "Double-DID") %>%
          mutate(estimator = ifelse(estimator == "sDID", "Double-DID", "DID")) %>%
          arrange(estimator)
)

## plot
adj <- c(-0.15, 0.15); cols <- c("gray50", "black"); pchs <- c(21, 16)
cairo_pdf(filename = "../results/figures/figure4_malesky-main-effects_final.pdf", 
          height = 3, width = 6.5)
par(mfrow = c(1,2), mar = c(2, 2, 2, 2), oma = c(0, 3, 0, 0))
plot(1, 1, type = 'n', xlim = c(0.5, 2.5), ylim = c(-0.05, 0.25),
     xaxt = "n", main = 'Education and Cultural Program',
     xlab = "", ylab = "",
     cex.main = 0.9, las = 1, cex.lab = 0.9, cex.axis = 0.8)
abline(h = 0, col = 'gray', lty = 3, lwd = 1.5)

## pro4
for (i in 1:2) {
  lines(c(i, i), c(tmp[[1]]$CI90_LB[i], tmp[[1]]$CI90_UB[i]), col = cols[i], lwd = 2)
  points(i, tmp[[1]]$estimate[i], pch = pchs[i], col = cols[i], bg = 'white', cex = 1.2)
}
mtext(side = 2, at = 0.55, "ATT (90% Confidence Interval)", outer = TRUE, line = 1, cex = 0.9)
text(c(1, 2), c(0.25, 0.25), labels = c("DID", "Double DID"), pos = 1, cex = 0.85, col = cols)

## tap
plot(1, 1, type = 'n', xlim = c(0.5, 2.5), ylim = c(-0.25, 0.05), xaxt = "n", main = 'Tap Water',
    xlab = "", ylab = "",
    cex.main = 0.9, las = 1, cex.lab = 0.9, cex.axis = 0.8)
abline(h = 0, col = 'gray', lty = 3, lwd = 1.5)

for (i in 1:2) {
  lines(c(i, i), c(tmp[[2]]$CI90_LB[i], tmp[[2]]$CI90_UB[i]), col = cols[i], lwd = 2)
  points(i, tmp[[2]]$estimate[i], pch = pchs[i], col = cols[i], bg = 'white', cex = 1.2)
}
text(c(1, 2), c(0.05, 0.05), labels = c("DID", "Double DID"), pos = 1, cex = 0.85, col = cols)
dev.off()
