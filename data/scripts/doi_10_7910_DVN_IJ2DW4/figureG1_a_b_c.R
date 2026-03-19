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

## load data
load('./data/crime/hate_crime.RDS')
covars_crime <- readRDS('./data/crime/covars_crime.RDS')

### Figure G1a. Minority victory effects on (non-hate) equivalent crimes 3 months after general election
main <- rdrobust(not_hate_crimes_w_at_3m[, crime_rate],
                 not_hate_crimes_w_at_3m[, victory_margin],
                 covs = not_hate_crimes_w_at_3m[,covars_crime,with=FALSE],
                 kernel = "triangular", p = 1, bwselect = "mserd",
                 nnmatch = 4,
                 cluster = not_hate_crimes_w_at_3m[,cluster])
bws <- main$bws[1]

mainp <- rdplot(not_hate_crimes_w_at_3m[(abs(victory_margin) <= bws),
                                        crime_rate],
                not_hate_crimes_w_at_3m[(abs(victory_margin) <= bws),
                                        victory_margin],
                covs = not_hate_crimes_w_at_3m[(abs(victory_margin) <= bws),
                                               covars_crime,
                                               with = FALSE],
                p = 1,
                covs_eval = 'mean',
                scale = 1,
                kernel = "triangular",
                x.label = "Ethnic minority victory margin",
                y.label=paste('Equivalent crimes per 1000 residents',
                              "3 months after general election",
                              sep='\n'), title = " ",
                col.lines = '#0000FF', col.dots = '#666666')

# compute confidence intervals for predicted values of local regression
crime_main_l <- not_hate_crimes_w_at_3m[(
  victory_margin <= 0 &
    victory_margin >= -bws)]
crime_main_r <- not_hate_crimes_w_at_3m[(
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
                              crime_main_l[, victory_margin], 
                              crime_main_l[, crime_rate],
                              x_pred_l, 0.05, unname(intercept_l$rdplot_y), unname(slope_l)))
ci_l <- cbind(ci_l, x_pred_l)

ci_r <- data.frame(ci_of_mean(main$N_h[2], crime_main_r[,victory_margin], crime_main_r[,crime_rate],
                              x_pred_r , 0.05, unname(intercept_r$rdplot_y), unname(slope_r)))

ci_r <- cbind(ci_r, x_pred_r)


mainp$rdplot +
  geom_ribbon(data=ci_l,aes(x=x_pred_l,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  geom_ribbon(data=ci_r,aes(x=x_pred_r,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  ggplot2::annotate(geom = "text", x=5, y=1.85, label=paste('N =', main$N_h[1] + main$N_h[2]), size = 3.5) +
  ggplot2::annotate(geom = "text", x=5, y=1.7,
                    label=paste('Avg. Bin Length =', round(((mainp$bin_avg[1]*mainp$J[1] + mainp$bin_avg[2]*mainp$J[2])/(mainp$J[1]+mainp$J[2])),1)), size = 3.5) +
  theme(plot.title = element_text(size=18, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size =14),
        axis.title.y = element_text(size =14),
        axis.text.y = element_text(size = 12))
ggsave(file = './output/figures/figureG1a.pdf', width=6, height=4.85)



### Figure G1b. Minority victory effects on (non-hate) equivalent crimes 1-9 months after general election
dataset <- list(not_hate_crimes_w_at_1m, not_hate_crimes_w_at_2m, not_hate_crimes_w_at_3m, not_hate_crimes_w_at_4m,
                not_hate_crimes_w_at_5m, not_hate_crimes_w_at_6m, not_hate_crimes_w_at_7m, not_hate_crimes_w_at_8m,
                not_hate_crimes_w_at_9m)

coef <- c()
se <- c()
p_value <- c()
cil <- c()
cir <- c()
mean_control <- c()
std_effect <- c()
std_cil <- c()
std_cir <- c()
bw <-  c()
nl <- c()
nr <- c()
Nl <- c()
Nr <- c()
month <- c()

for (i in seq(1,length(dataset),1)) {
  
  if (i <= 2) {
    nnmatch = 3
  } else {
    nnmatch = i+1
  }
  
  data <- dataset[[i]]
  
  main <- rdrobust(data[, crime_rate],
                   data[, victory_margin], p = 1,
                   kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                   cluster = data[, cluster])
  
  main_covar <- rdrobust(data[, crime_rate],
                         data[, victory_margin],
                         covs = data[, covars_crime, with = FALSE],
                         p = 1,
                         kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                         cluster = data[, cluster])
  
  # estimates
  coef <- c(coef, main$coef[1,1])
  coef <- c(coef, main_covar$coef[1,1])
  se <- c(se, main$se[1,1])
  se <- c(se, main_covar$se[1,1])
  p_value <- c(p_value, main$pv[3,1])
  p_value <- c(p_value, main_covar$pv[3,1])
  cil <- c(cil, main$ci[3,1])
  cil <- c(cil, main_covar$ci[3,1])
  cir <- c(cir, main$ci[3,2])
  cir <- c(cir, main_covar$ci[3,2])
  mean_control <- c(mean_control, main$beta_p_l[1])
  mean_control <- c(mean_control, main_covar$beta_p_l[1])
  std_effect <- c(std_effect, main$coef[1,1]/(data[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                                   sd(crime_rate, na.rm = TRUE)]))
  std_effect <- c(std_effect, main_covar$coef[1,1]/(data[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                         sd(crime_rate, na.rm = TRUE)]))
  std_cil <- c(std_cil, main$ci[3,1]/(data[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                           sd(crime_rate, na.rm = TRUE)]))
  std_cil <- c(std_cil, main_covar$ci[3,1]/(data[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                 sd(crime_rate, na.rm = TRUE)]))
  
  std_cir <- c(std_cir, main$ci[3,2]/(data[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                           sd(crime_rate, na.rm = TRUE)]))
  std_cir <- c(std_cir, main_covar$ci[3,2]/(data[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                 sd(crime_rate, na.rm = TRUE)]))
  bw <-  c(bw, main$bws[1])
  bw <-  c(bw, main_covar$bws[1])
  nl <- c(nl, main$N_h[1])
  nl <- c(nl, main_covar$N_h[1])
  nr <- c(nr, main$N_h[2])
  nr <- c(nr, main_covar$N_h[2])
  Nl <- c(Nl, main$N[1])
  Nl <- c(Nl, main_covar$N[1])
  Nr <- c(Nr, main$N[2])
  Nr <- c(Nr, main_covar$N[2])
  month <- c(month, rep(i,2))
  
} 

# store estimates and save for supplementary information tables/figures
estimates <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                            std_effect=std_effect, std_cil=std_cil, std_cir=std_cir,
                            bw=bw, n=nl+nr, N=Nl+Nr,
                            cov = rep(c('no', 'yes'), length(coef)/2),
                            month = month)
estimates <- data.table(estimates) 
saveRDS(estimates, file = './output/estimates/crime_placebo_estimates.RDS')


ggplot(estimates[cov=='yes'],
       aes(x=month, y=coef)) + geom_point(color = "#2f3132") +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, color = "#2f3132") +
  geom_hline(yintercept=0, linetype='longdash', linewidth=0.2) +
  scale_x_continuous(breaks = c(1:9)) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on',
             "equivalent crimes per 1000 residents",
             sep='\n')) +
  #ggtitle("Main effects across time") +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figureG1b.pdf', width=6, height=4.85)



### Figure G1c. Comparison of minority victory effects on hate crimes and (non-hate) equivalent crimes 1-9 months after general election
estimates_main <- readRDS(file = './output/estimates/crime_estimates.RDS')
estimates_main[,type:='main effect']

estimates_placebo <- readRDS(file = './output/estimates/crime_placebo_estimates.RDS')
estimates_placebo[,type:='placebo effect']
estimates_all <- rbind(estimates_main, estimates_placebo)

ggplot(estimates_all[cov=='yes'],
       aes(x=month, y=std_effect, color=type)) + geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=std_cil, ymax=std_cir), width=.1, position=position_dodge(0.2)) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  scale_x_continuous(breaks = c(1:10)) +
  scale_color_manual(values=c("#0000FF", "#2f3132")) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on',
             "hate crimes and equivalent crimes",
             "(standard deviations)",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figureG1c.pdf', width=6, height=4.85)
