rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(dplyr)
library(tidyr)
library(rdrobust)
library(ggplot2)

##### function to add CIs in RD plot
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
##########

## load and subset data to white respondents in England, Wales
load(file='./data/attitudes/attitudes.rds')
bes_rdd <- bes_rdd[grepl('^(E|W)', ons_id) & white==1]
covars_attitudes <- readRDS('./data/attitudes/covars_attitudes.RDS')

# in-text number
bes_rdd[,.N]
nrow(bes_rdd[, .N, by=.(ons_id, election)])/662 #note 662 is in-sample number of constituency-election years (see line 55 in figureE1_a_b_c.R)


### Figure 2
# compute RD estimates
main <- rdrobust(bes_rdd[, j05],
                 bes_rdd[, victory_margin],
                 covs = bes_rdd[, covars_attitudes, with=F],
                 kernel = "triangular", p = 1, bwselect = "mserd",
                 cluster = bes_rdd[, cluster])

# store optimal bandwidth
bws <- main$bws[1]

# compute RD plot within optimal bandwidth
mainp <- rdplot(bes_rdd[(abs(victory_margin) <= bws),
                        j05],
                bes_rdd[(abs(victory_margin) <= bws),
                        victory_margin],
                covs = bes_rdd[(abs(victory_margin) <= bws),
                               covars_attitudes, with=F],
                p = 1,
                covs_eval = 'mean',
                scale = 1,
                kernel = "triangular",
                x.label = "Ethnic minority victory margin",
                y.label=paste('Inclusionary attitudes towards',
                              "immigrants after general election",
                              sep='\n'), title = " ",
                col.lines = '#0000FF', col.dots = '#666666')

# compute confidence intervals for predicted values of local regression
attitudes_main_l <- bes_rdd[ (
  victory_margin <= 0 &
    victory_margin >= -bws)]
attitudes_main_r <- bes_rdd[(
  victory_margin >= 0 &
    victory_margin <= bws)]

x_pred_l <- seq(min(mainp$vars_poly[,1]), 0, -min(mainp$vars_poly[,1])/500)
x_pred_r <- seq(0, max(mainp$vars_poly[,1]), max(mainp$vars_poly[,1])/500)

intercept_l <- mainp$vars_poly[mainp$vars_poly$rdplot_x == 0,][1,]
intercept_r <- mainp$vars_poly[mainp$vars_poly$rdplot_x == 0,][2,]
min_l <- head(mainp$vars_poly, 1)
slope_l <- (intercept_l$rdplot_y - min_l$rdplot_y )/(intercept_l$rdplot_x - min_l$rdplot_x )
max_r <- tail(mainp$vars_poly, 1)
slope_r <- (intercept_r$rdplot_y - max_r$rdplot_y )/(intercept_r$rdplot_x -max_r$rdplot_x )


ci_l <- data.frame(ci_of_mean(main$N_h[1], 
                              attitudes_main_l[, victory_margin], 
                              attitudes_main_l[, j05],
                              x_pred_l, 0.05, unname(intercept_l$rdplot_y), unname(slope_l)))
ci_l <- cbind(ci_l, x_pred_l)

ci_r <- data.frame(ci_of_mean(main$N_h[2], attitudes_main_r[,victory_margin],
                              attitudes_main_r[,j05],
                              x_pred_r , 0.05, unname(intercept_r$rdplot_y), unname(slope_r)))

ci_r <- cbind(ci_r, x_pred_r)


# compute and save RD plot w/CIs
mainp$rdplot +
  geom_ribbon(data=ci_l,aes(x=x_pred_l,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  geom_ribbon(data=ci_r,aes(x=x_pred_r,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  ggplot2::annotate(geom = "text", x=5.5, y=0.6, label=paste('N =', main$N_h[1] + main$N_h[2]), size = 3.5) +
  ggplot2::annotate(geom = "text", x=5.5, y=0.55,
                    label=paste('Avg. Bin Length =', round(((mainp$bin_avg[1]*mainp$J[1] + mainp$bin_avg[2]*mainp$J[2])/(mainp$J[1]+mainp$J[2])),1)), size = 3.5) +
  theme(plot.title = element_text(size=18, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size =14),
        axis.title.y = element_text(size =14),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figure2.pdf', width=6, height=4.85)
