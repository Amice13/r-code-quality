rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
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
load(file='./data/attitudes/attitudes.rds')
bes_rdd <- bes_rdd[grepl('^(E|W)', ons_id) & white==1]
covars_attitudes <- readRDS('./data/attitudes/covars_attitudes.RDS')

#### Figure I1a: RD effects on ideology (placebo outcome)
main <- rdrobust(bes_rdd[, left_right],
                 bes_rdd[, victory_margin],
                 covs = bes_rdd[, covars_attitudes, with=F],
                 kernel = "triangular", p = 1, bwselect = "mserd",
                 cluster = bes_rdd[, cluster])

bws <- main$bws[1]
mainp <- rdplot(bes_rdd[(abs(victory_margin) <= bws),
                        left_right],
                bes_rdd[(abs(victory_margin) <= bws),
                        victory_margin],
                covs = bes_rdd[(abs(victory_margin) <= bws),
                               covars_attitudes, with=F],
                p = 1,
                covs_eval = 'mean',
                scale = 1,
                kernel = "triangular",
                x.label = "Ethnic minority victory margin",
                y.label=paste('Left-right views',
                              "after general election",
                              sep='\n'), title = " ",
                col.lines = '#0000FF', col.dots = '#666666')

# compute confidence intervals for predicted values of local regression
bes_rdd_complete <- bes_rdd[complete.cases(bes_rdd[, covars_attitudes, with = FALSE])]
attitudes_main_l <- bes_rdd_complete[ (
  victory_margin <= 0 &
    victory_margin >= -bws)]
attitudes_main_r <- bes_rdd_complete[(
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
                              attitudes_main_l[, left_right],
                              x_pred_l, 0.05, unname(intercept_l$rdplot_y), unname(slope_l)))
ci_l <- cbind(ci_l, x_pred_l)

ci_r <- data.frame(ci_of_mean(main$N_h[2], attitudes_main_r[,victory_margin],
                              attitudes_main_r[,left_right],
                              x_pred_r , 0.05, unname(intercept_r$rdplot_y), unname(slope_r)))

ci_r <- cbind(ci_r, x_pred_r)

mainp$rdplot +
  geom_ribbon(data=ci_l,aes(x=x_pred_l,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  geom_ribbon(data=ci_r,aes(x=x_pred_r,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  ggplot2::annotate(geom = "text", x=5.5, y=-0.8, label=paste('N =', main$N_h[1] + main$N_h[2]), size = 3.5) +
  ggplot2::annotate(geom = "text", x=5.5, y=-0.85,
                    label=paste('Avg. Bin Length =', round(((mainp$bin_avg[1]*mainp$J[1] + mainp$bin_avg[2]*mainp$J[2])/(mainp$J[1]+mainp$J[2])),1)), size = 3.5) +
  theme(plot.title = element_text(size=18, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size =14),
        axis.title.y = element_text(size =14),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureI1a.pdf', width=6, height=4.85)




#### Figure I1b: Comparison of main and placebo RD effects
## compute estimates on placebo outcome
placebo_outcomes <- c('left_right')

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

for (i in placebo_outcomes) {
  
  main <- rdrobust(bes_rdd[[i]],
                   bes_rdd$victory_margin,
                   kernel = "triangular", p = 1, bwselect = "mserd",
                   cluster = bes_rdd$cluster)
  
  main_complete <- rdrobust(bes_rdd_complete[[i]],
                            bes_rdd_complete$victory_margin,
                            kernel = "triangular", p = 1, bwselect = "mserd",
                            cluster = bes_rdd_complete$cluster)
  
  main_covar <- rdrobust(bes_rdd[[i]],
                         bes_rdd$victory_margin,
                         covs = bes_rdd[, covars_attitudes, with=F],
                         kernel = "triangular", p = 1, bwselect = "mserd",
                         cluster = bes_rdd$cluster)
  
  
  coef <- c(coef, main$coef[1,1])
  coef <- c(coef, main_complete$coef[1,1])
  coef <- c(coef, main_covar$coef[1,1])
  se <- c(se, main$se[3,1])
  se <- c(se, main_complete$se[3,1])
  se <- c(se, main_covar$se[3,1])
  p_value <- c(p_value, main$pv[3,1])
  p_value <- c(p_value, main_complete$pv[3,1])
  p_value <- c(p_value, main_covar$pv[3,1])
  cil <- c(cil, main$ci[3,1])
  cil <- c(cil, main_complete$ci[3,1])
  cil <- c(cil, main_covar$ci[3,1])
  cir <- c(cir, main$ci[3,2])
  cir <- c(cir, main_complete$ci[3,2])
  cir <- c(cir, main_covar$ci[3,2])
  mean_control <- c(mean_control, main$beta_p_l[1])
  mean_control <- c(mean_control, main_complete$beta_p_l[1])
  mean_control <- c(mean_control, main_covar$beta_p_l[1])
  std_effect <- c(std_effect, main$coef[1,1]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                                      sd(get(i), na.rm = TRUE)]))
  std_effect <- c(std_effect, main_complete$coef[1,1]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                        sd(get(i), na.rm = TRUE)]))
  
  std_effect <- c(std_effect, main_covar$coef[1,1]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                                     sd(get(i), na.rm = TRUE)]))
  std_cil <- c(std_cil, main$ci[3,1]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                              sd(get(i), na.rm = TRUE)]))
  std_cil <- c(std_cil, main_complete$ci[3,1]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                sd(get(i), na.rm = TRUE)]))
  
  std_cil <- c(std_cil, main_covar$ci[3,1]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                             sd(get(i), na.rm = TRUE)]))
  
  std_cir <- c(std_cir, main$ci[3,2]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                              sd(get(i), na.rm = TRUE)]))
  std_cir <- c(std_cir, main_complete$ci[3,2]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                sd(get(i), na.rm = TRUE)]))
  std_cir <- c(std_cir, main_covar$ci[3,2]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                             sd(get(i), na.rm = TRUE)]))
  bw <-  c(bw, main$bws[1])
  bw <-  c(bw, main_complete$bws[1])
  bw <-  c(bw, main_covar$bws[1])
  nl <- c(nl, main$N_h[1])
  nl <- c(nl, main_complete$N_h[1])
  nl <- c(nl, main_covar$N_h[1])
  nr <- c(nr, main$N_h[2])
  nr <- c(nr, main_complete$N_h[2])
  nr <- c(nr, main_covar$N_h[2])
  Nl <- c(Nl, main$N[1])
  Nl <- c(Nl, main_complete$N[1])
  Nl <- c(Nl, main_covar$N[1])
  Nr <- c(Nr, main$N[2])
  Nr <- c(Nr, main_complete$N[2])
  Nr <- c(Nr, main_covar$N[2])
  
}

estimates_placebo <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                            std_effect=std_effect, std_cil=std_cil, std_cir=std_cir,
                            bw=bw, n=nl+nr, N=Nl+Nr,
                            cov = rep(c('no','no','yes'),1),
                            sample = rep(c('full', 'complete cases', 'complete cases'),1))
estimates_placebo <- data.table(estimates_placebo)
estimates_placebo[,type:='placebo effect']

### compute estimates on main outcome
main_outcomes <- c('j05')

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

for (i in main_outcomes) {
  
  main <- rdrobust(bes_rdd[[i]],
                   bes_rdd$victory_margin,
                   kernel = "triangular", p = 1, bwselect = "mserd",
                   cluster = bes_rdd$cluster)
  
  main_complete <- rdrobust(bes_rdd_complete[[i]],
                            bes_rdd_complete$victory_margin,
                            kernel = "triangular", p = 1, bwselect = "mserd",
                            cluster = bes_rdd_complete$cluster)
  
  main_covar <- rdrobust(bes_rdd[[i]],
                         bes_rdd$victory_margin,
                         covs = bes_rdd[, covars_attitudes, with=F],
                         kernel = "triangular", p = 1, bwselect = "mserd",
                         cluster = bes_rdd$cluster)
  
  
  coef <- c(coef, main$coef[1,1])
  coef <- c(coef, main_complete$coef[1,1])
  coef <- c(coef, main_covar$coef[1,1])
  se <- c(se, main$se[3,1])
  se <- c(se, main_complete$se[3,1])
  se <- c(se, main_covar$se[3,1])
  p_value <- c(p_value, main$pv[3,1])
  p_value <- c(p_value, main_complete$pv[3,1])
  p_value <- c(p_value, main_covar$pv[3,1])
  cil <- c(cil, main$ci[3,1])
  cil <- c(cil, main_complete$ci[3,1])
  cil <- c(cil, main_covar$ci[3,1])
  cir <- c(cir, main$ci[3,2])
  cir <- c(cir, main_complete$ci[3,2])
  cir <- c(cir, main_covar$ci[3,2])
  mean_control <- c(mean_control, main$beta_p_l[1])
  mean_control <- c(mean_control, main_complete$beta_p_l[1])
  mean_control <- c(mean_control, main_covar$beta_p_l[1])
  std_effect <- c(std_effect, main$coef[1,1]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                                      sd(get(i), na.rm = TRUE)]))
  std_effect <- c(std_effect, main_complete$coef[1,1]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                        sd(get(i), na.rm = TRUE)]))
  
  std_effect <- c(std_effect, main_covar$coef[1,1]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                                     sd(get(i), na.rm = TRUE)]))
  std_cil <- c(std_cil, main$ci[3,1]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                              sd(get(i), na.rm = TRUE)]))
  std_cil <- c(std_cil, main_complete$ci[3,1]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                sd(get(i), na.rm = TRUE)]))
  
  std_cil <- c(std_cil, main_covar$ci[3,1]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                             sd(get(i), na.rm = TRUE)]))
  
  std_cir <- c(std_cir, main$ci[3,2]/(bes_rdd[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0)),
                                              sd(get(i), na.rm = TRUE)]))
  std_cir <- c(std_cir, main_complete$ci[3,2]/(bes_rdd_complete[((victory_margin >= -main_complete$bws[1,1]) & (victory_margin < 0)),
                                                                sd(get(i), na.rm = TRUE)]))
  std_cir <- c(std_cir, main_covar$ci[3,2]/(bes_rdd_complete[((victory_margin >= -main_covar$bws[1,1]) & (victory_margin < 0)),
                                                             sd(get(i), na.rm = TRUE)]))
  bw <-  c(bw, main$bws[1])
  bw <-  c(bw, main_complete$bws[1])
  bw <-  c(bw, main_covar$bws[1])
  nl <- c(nl, main$N_h[1])
  nl <- c(nl, main_complete$N_h[1])
  nl <- c(nl, main_covar$N_h[1])
  nr <- c(nr, main$N_h[2])
  nr <- c(nr, main_complete$N_h[2])
  nr <- c(nr, main_covar$N_h[2])
  Nl <- c(Nl, main$N[1])
  Nl <- c(Nl, main_complete$N[1])
  Nl <- c(Nl, main_covar$N[1])
  Nr <- c(Nr, main$N[2])
  Nr <- c(Nr, main_complete$N[2])
  Nr <- c(Nr, main_covar$N[2])
  
}

estimates_main <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                                std_effect=std_effect, std_cil=std_cil, std_cir=std_cir,
                                bw=bw, n=nl+nr, N=Nl+Nr,
                                cov = rep(c('no','no','yes'),1),
                                sample = rep(c('full', 'complete cases', 'complete cases'),1))
estimates_main <- data.table(estimates_main)
estimates_main[,type:='main effect']


estimates_all <- rbind(estimates_main, estimates_placebo, fill=TRUE)
estimates_all[cov=='yes', cov:='Controls: yes']
estimates_all[cov=='no', cov:='Controls: no']

ggplot(estimates_all[sample=='complete cases'],
       aes(x=cov, y=std_effect, color=type)) + geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=std_cil, ymax=std_cir), width=.1, position=position_dodge(0.2)) +
  geom_hline(yintercept=0, linetype='longdash', linewidth=0.2) +
  #geom_line(aes(x=month, y=(nl+nr))) +
  scale_color_manual(values=c("#0000FF", "#2f3132")) +
  xlab(NULL) +
  ylab(paste('Minority victory effects on',
             "inclusionary attitudes towards",
             "immigrants & placebo attitudes (std dev)",
             sep='\n')) +
  #ggtitle("Main & placebo effects across time") +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave('./output/figures/figureI1b.pdf', width=6, height=4.85)

