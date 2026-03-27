
##
## Script to summarise ATT results
##
##


## load packages
require(tidyverse)

## ------------------------------------ ##
## define functions
## ------------------------------------ ##
summarize_augsynth <- function(dat) {
  time_vec <- dat$Time
  att_vec  <- dat$Estimate
  ci_ub    <- att_vec + 1.96 * dat$Std.Error
  ci_lb    <- att_vec - 1.96 * dat$Std.Error
  return(list(time_vec = time_vec, att_vec = att_vec,
              ci_ub = ci_ub, ci_lb = ci_lb))
}

summarize_ddid <- function(dat) {
  time_vec <- dat$time
  att_vec  <- dat$estimate
  ci_ub    <- att_vec + 1.96 * dat$std.error
  ci_lb    <- att_vec - 1.96 * dat$std.error
  return(list(time_vec = time_vec, att_vec = att_vec,
              ci_ub = ci_ub, ci_lb = ci_lb
  ))
}

summarise_gsynth <- function(dat) {
    time_vec <- dat$Time
    att_vec  <- dat$ATT
    ci_ub    <- dat$CI.lower
    ci_lb    <- dat$CI.upper
    return(list(time_vec = time_vec, att_vec = att_vec,
                ci_ub = ci_ub, ci_lb = ci_lb))
}


## plot function
plot_att <- function(dat, estimator, ...) {
  ## define constants
  gray_alpha <- rgb(128,128,128, maxColorValue = 255, alpha = 50)

  ## extract summary infomation
  if (estimator == "augsynth") {
    res_summary <- summarize_augsynth(dat)
  } else if (estimator == "double_did") {
    res_summary <- summarize_ddid(dat)
  } else if (estimator == 'gsynth') {
    res_summary <- summarise_gsynth(dat)
  }

  time_vec <- res_summary$time_vec
  att_vec  <- res_summary$att_vec
  ci_ub    <- res_summary$ci_ub
  ci_lb    <- res_summary$ci_lb

  plot(1, 1, type = 'n', xlim = c(min(time_vec), max(time_vec)),
       xlab = "", ylab = "",  ...)
  title(xlab = "Time", ylab = "SA-ATT", line = 2.1)

  ## plot markers
  abline(v = 0, col = 'red', lty = 2)
  abline(h = 0, col = 'gray60', lty = 2)

  ## plot CI
  polygon(
    c(time_vec, rev(time_vec)),
    c(ci_ub, rev(ci_lb)),
    border = NA, col = gray_alpha
  )

  ## plot point estimates
  lines(time_vec, att_vec, type = 'o', pch = 16)
}


# Plot for pre-treatment trends
plot_pretest <- function(dat, ylim, ...) {
  plot(dat$time_to_treat, dat$estimate, ylim = ylim,
    xlab = "", ylab = "95% Standardized Equivalence CI",
    type = 'n', las = 1, cex.axis = 0.9, ...)
  abline(h = 0, col = 'lightgray', lty = 3, lwd = 1.5)
  for (i in 1:length(dat$time_to_treat)) {
    arrows(dat$time_to_treat[i], dat$EqCI95_LB[i],
           dat$time_to_treat[i], dat$EqCI95_UB[i],
           length = 0.03, angle = 90, code = 3, col = 'gray20', lwd = 1.5)
  }
  title(xlab = "Time Relative to Treatment Assignment", line = 2, cex.lab = 0.85)
}


## ------------------------------------ ##
## load results
## ------------------------------------ ##

ddid            <- readRDS('../results/application/11-double_did_paglayan_final.rds')
augsynth_expend <- readRDS('../results/application/12-augsynth_paglayan_expenditure.rds')
augsynth_salary <- readRDS('../results/application/13-augsynth_paglayan_salary.rds')
gsynth_att      <- readRDS('../results/application/14-gsynth_paglayan.rds')


## ------------------------------------ ##
## plot pre-treatment test
## ------------------------------------ ##

dt_salary <- ddid$check[[2]]$plot[[1]]$dat_plot
dt_expend <- ddid$check[[1]]$plot[[1]]$dat_plot

cairo_pdf(filename = "../results/figures/figureA7_10_paglayan_pretest_final.pdf",
          height = 4, width = 7, bg = 'transparent')
par(mfrow = c(1, 2), mar = c(3.8, 4, 3, 1), pty = 's')
plot_pretest(dt_expend, ylim = c(-0.2, 0.2), main = "Expenditure")
plot_pretest(dt_salary, ylim = c(-0.2, 0.2), main = "Salary")
dev.off()


## ------------------------------------ ##
## main
## ------------------------------------ ##


## prepare data
ddid_expend <- bind_rows(
  ddid$check[[1]]$estimate %>%
    mutate(time = -lag) %>%
    select(estimate = estimate_orig, std.error = std.error_orig, time),
  ddid$est[[1]]$estimate  %>%
    filter(estimator == "SA-Double-DID") %>%
    select(estimate, std.error, time = lead)
) %>% arrange(time) %>% filter(time >= -3)


ddid_salary <- bind_rows(
  ddid$check[[2]]$estimate %>%
    mutate(time = -lag) %>%
    select(estimate = estimate_orig, std.error = std.error_orig, time),
  ddid$est[[2]]$estimate  %>%
    filter(estimator == "SA-Double-DID") %>%
    select(estimate, std.error, time = lead)
) %>% arrange(time) %>% filter(time >= -3)



##
## combined
##
pdf(file = "../results/figures/figureA8_23_paglayan_combined_final.pdf", height = 5.2, width = 7.5)
par(mfrow = c(2, 3), mar = c(3, 3, 2, 1), oma = c(0, 3, 1, 0))
## Expenditure
## double did
plot_att(ddid_expend, estimator = 'double_did', ylim = c(-0.11, 0.12),
         main = "Double DID")
mtext(text = "Expenditure", side = 2, font = 2, outer = TRUE, adj = 0.82, line = 1.1)
## gsynth
plot_att(gsynth_att[[1]], estimator = "gsynth", ylim = c(-0.11, 0.12), main = "gsynth")
## augsynth
plot_att(augsynth_expend$att %>% filter(Level == "Average" & Time >= -3 & Time <= 10),
          estimator = "augsynth",
         ylim = c(-0.11, 0.12), main = "augsynth")

## salary
plot_att(ddid_salary, estimator = 'double_did', ylim = c(-0.11, 0.12),
          main = "Double DID")
mtext(text = "Salary", side = 2, font = 2, outer = TRUE, adj = 0.25, line = 1.1)
plot_att(gsynth_att[[2]], estimator = "gsynth", ylim = c(-0.11, 0.12), main = "gsynth")
plot_att(augsynth_salary$att %>% filter(Level == "Average" & Time >= -3 & Time <= 10),
         estimator = "augsynth",
         ylim = c(-0.11, 0.12), main = "augsynth")
dev.off()
