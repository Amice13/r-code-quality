rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(ggplot2)
library(tidyr)
library(rdrobust)

####### function to print CIs in RD plot
ci_of_mean <- function(n, X_fit, Y_fit, X_pred, alpha, a, b) {
  #  https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
  X_fit_bar <- mean(X_fit)
  Y_fit_hat <- a + X_fit *b
  
  Y_fit <- Y_fit %>% replace_na(mean(Y_fit, na.rm = TRUE))
  
  MSE <- sum((Y_fit_hat - Y_fit) ^2 ) / (n-2)
  t_val <- qt((1-alpha/2), df=n-2)
  SE <-  sqrt(MSE * ((1/n) + (X_pred - X_fit_bar)^2 / sum((X_fit - X_fit_bar)^2)))
  Y_pred_hat <- a + X_pred *b
  upper <- Y_pred_hat + t_val * SE
  lower <- Y_pred_hat - t_val * SE
  return(cbind(Y_pred_hat, lower, upper))
}
#########

## load data
data <- readRDS('./data/campaigns/campaign_issues.RDS')

## Figure H2a: discontinuity in UKIP running?
main <- rdrobust(data[, ukip],
                 data[, minority_victory_margin],
                 kernel = "triangular", p = 1, bwselect = "mserd")
bws <- main$bws[1]
mainp <- rdplot(data[(abs(minority_victory_margin) <= bws),
                           ukip],
                data[(abs(minority_victory_margin) <= bws),
                           minority_victory_margin],
                p = 1,
                kernel = "triangular",
                x.label = "Ethnic minority victory margin",
                y.label='Probability UKIP running', title = " ",
                col.lines = '#0000FF', col.dots = '#666666')

crime_main_l <- data[(
  minority_victory_margin <= 0 &
    minority_victory_margin >= -bws)]
crime_main_r <- data[(
  minority_victory_margin >= 0 &
    minority_victory_margin <= bws)]

x_pred_l <- seq(min(mainp$vars_poly[,1]), 0, -min(mainp$vars_poly[,1])/500)
x_pred_r <- seq(0, max(mainp$vars_poly[,1]), max(mainp$vars_poly[,1])/500)

intercept_l <- mainp$vars_poly[mainp$vars_poly$rdplot_x == 0,][1,]
intercept_r <- mainp$vars_poly[mainp$vars_poly$rdplot_x == 0,][2,]
min_l <- head(mainp$vars_poly, 1)
slope_l <- (intercept_l$rdplot_y - min_l$rdplot_y )/(intercept_l$rdplot_x - min_l$rdplot_x )
max_r <- tail(mainp$vars_poly, 1)
slope_r <- (intercept_r$rdplot_y - max_r$rdplot_y )/(intercept_r$rdplot_x -max_r$rdplot_x )


ci_l <- data.frame(ci_of_mean(main$N_h[1], 
                              crime_main_l[, minority_victory_margin], 
                              crime_main_l[, ukip],
                              x_pred_l, 0.05, unname(intercept_l$rdplot_y), unname(slope_l)))
ci_l <- cbind(ci_l, x_pred_l)

ci_r <- data.frame(ci_of_mean(main$N_h[2], crime_main_r[,minority_victory_margin], crime_main_r[,ukip],
                              x_pred_r , 0.05, unname(intercept_r$rdplot_y), unname(slope_r)))

ci_r <- cbind(ci_r, x_pred_r)

mainp$rdplot +
  geom_ribbon(data=ci_l,aes(x=x_pred_l,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  geom_ribbon(data=ci_r,aes(x=x_pred_r,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  ggplot2::annotate(geom = "text", x=10, y=0.5, label=paste('RD estimate =', round(main$Estimate[1],2)), size = 3.5) +
  ggplot2::annotate(geom = "text", x=10, y=0.45,
                    label=paste('p-value =', round(main$pv[3],2)), size = 3.5) +
  theme(plot.title = element_text(size=18, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size =14),
        axis.title.y = element_text(size =14),
        axis.text.y = element_text(size = 12))
ggsave(file = './output/figures/figureH2a.pdf', width=6, height=4.85)


## Figure H2b: discontinuity in UKIP vote share?

main <- rdrobust(data[, ukip_vs],
                 data[, minority_victory_margin],
                 kernel = "triangular", p = 1, bwselect = "mserd")
bws <- main$bws[1]
mainp <- rdplot(data[(abs(minority_victory_margin) <= bws),
                           ukip_vs],
                data[(abs(minority_victory_margin) <= bws),
                           minority_victory_margin],
                p = 1,
                kernel = "triangular",
                x.label = "Ethnic minority victory margin",
                y.label='UKIP vote share', title = " ",
                col.lines = '#0000FF', col.dots = '#666666')

crime_main_l <- data[(
  minority_victory_margin <= 0 &
    minority_victory_margin >= -bws)]
crime_main_r <- data[(
  minority_victory_margin >= 0 &
    minority_victory_margin <= bws)]

x_pred_l <- seq(min(mainp$vars_poly[,1]), 0, -min(mainp$vars_poly[,1])/500)
x_pred_r <- seq(0, max(mainp$vars_poly[,1]), max(mainp$vars_poly[,1])/500)

intercept_l <- mainp$vars_poly[mainp$vars_poly$rdplot_x == 0,][1,]
intercept_r <- mainp$vars_poly[mainp$vars_poly$rdplot_x == 0,][2,]
min_l <- head(mainp$vars_poly, 1)
slope_l <- (intercept_l$rdplot_y - min_l$rdplot_y )/(intercept_l$rdplot_x - min_l$rdplot_x )
max_r <- tail(mainp$vars_poly, 1)
slope_r <- (intercept_r$rdplot_y - max_r$rdplot_y )/(intercept_r$rdplot_x -max_r$rdplot_x )


ci_l <- data.frame(ci_of_mean(main$N_h[1], 
                              crime_main_l[, minority_victory_margin], 
                              crime_main_l[, ukip_vs],
                              x_pred_l, 0.05, unname(intercept_l$rdplot_y), unname(slope_l)))
ci_l <- cbind(ci_l, x_pred_l)

ci_r <- data.frame(ci_of_mean(main$N_h[2], crime_main_r[,minority_victory_margin], crime_main_r[,ukip_vs],
                              x_pred_r , 0.05, unname(intercept_r$rdplot_y), unname(slope_r)))

ci_r <- cbind(ci_r, x_pred_r)

mainp$rdplot +
  geom_ribbon(data=ci_l,aes(x=x_pred_l,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  geom_ribbon(data=ci_r,aes(x=x_pred_r,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  ggplot2::annotate(geom = "text", x=-12, y=4, label=paste('RD estimate =', round(main$Estimate[1],2)), size = 3.5) +
  ggplot2::annotate(geom = "text", x=-12, y=3,
                    label=paste('p-value =', round(main$pv[3],2)), size = 3.5) +
  theme(plot.title = element_text(size=18, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size =14),
        axis.title.y = element_text(size =14),
        axis.text.y = element_text(size = 12))
ggsave(file = './output/figures/figureH2b.pdf', width=6, height=4.85)
