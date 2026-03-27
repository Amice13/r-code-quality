rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(tidyr)
library(dplyr)
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
sentiment <- readRDS('./data/media/media.RDS')
covars_media <- readRDS('./data/media/covars_media.RDS')
sentiment_placebo <- readRDS('./data/media/media_placebo.RDS')
sentiment_placebo[ ,cluster:=paste(election,pcon18cd,sep='_')]
sentiment_placebo[ , party_left:=ifelse(party_political_position=='left',1,0)]
sentiment_placebo[is.na(young) & region_name!='Scotland', young:=young_share]
sentiment_placebo[is.na(grade_c1) & region_name!='Scotland', grade_c1:=gradea_c1]

### Figure J.1a: Ethnic minority victory effects on media tone of placebo groups
## Effect on proportion of negative mentions about placebo groups
i <- 3
j <- 'prop_negative'

main <- rdrobust(sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i) & !is.na(first_generation), get(j)],
                 sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), victory_margin],
                 covs = sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), covars_media, with = FALSE],
                 p = 1,
                 kernel = "triangular", bwselect = "mserd", nnmatch = 3,
                 cluster = sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])

bws <- main$bws[1]
mainp <- rdplot(sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i & abs(victory_margin) <= bws & !is.na(first_generation)),
                                  get(j)],
                sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i & abs(victory_margin) <= bws & !is.na(first_generation)),
                                  victory_margin],
                covs = sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i & abs(victory_margin) <= bws & !is.na(first_generation)),
                                         covars_media, with = FALSE],
                p = 1,
                covs_eval = 'mean',
                scale = 2,
                kernel = "triangular",
                x.label = "Ethnic minority victory margin",
                y.label=paste('Proportion of negative mentions',
                              "about placebo groups",
                              "3 months after general election",
                              sep='\n'), title = " ",
                col.lines = '#0000FF', col.dots = '#666666')

# compute confidence intervals for predicted values of local regression
sentiment_placebo_main_l <- sentiment_placebo[(region_name!='Scotland' &
                                                 as.numeric(as.character(period))<=i &
                                                 !is.na(first_generation) &
                                                 victory_margin <= 0 &
                                                 victory_margin >= -bws)]
sentiment_placebo_main_r <- sentiment_placebo[(region_name!='Scotland' &
                                                 as.numeric(as.character(period))<=i &
                                                 !is.na(first_generation) &
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
                              sentiment_placebo_main_l[, victory_margin], 
                              sentiment_placebo_main_l[, get(j)],
                              x_pred_l, 0.05, unname(intercept_l$rdplot_y), unname(slope_l)))
ci_l <- cbind(ci_l, x_pred_l)

ci_r <- data.frame(ci_of_mean(main$N_h[2], sentiment_placebo_main_r[,victory_margin], sentiment_placebo_main_r[,get(j)],
                              x_pred_r , 0.05, unname(intercept_r$rdplot_y), unname(slope_r)))

ci_r <- cbind(ci_r, x_pred_r)


pmain <-   mainp$rdplot +
  geom_ribbon(data=ci_l,aes(x=x_pred_l,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  geom_ribbon(data=ci_r,aes(x=x_pred_r,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  ggplot2::annotate(geom = "text", x=8.5, y=0.45, label=paste('N =', main$N_h[1] + main$N_h[2]), size = 3.5) +
  ggplot2::annotate(geom = "text", x=8.5, y=0.4,
                    label=paste('Avg. Bin Length =', round(((mainp$bin_avg[1]*mainp$J[1] + mainp$bin_avg[2]*mainp$J[2])/(mainp$J[1]+mainp$J[2])),1)), size = 3.5) +
  ggtitle('Main effect') +   
  theme(plot.title = element_text(size=18, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size =14),
        axis.title.y = element_text(size =14),
        axis.text.y = element_text(size = 12)
  )

mainp$rdplot +
  geom_ribbon(data=ci_l,aes(x=x_pred_l,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  geom_ribbon(data=ci_r,aes(x=x_pred_r,ymin=lower,ymax=upper),alpha=0.3, fill='#0000FF') +
  ggplot2::annotate(geom = "text", x=8.5, y=0.5, label=paste('N =', main$N_h[1] + main$N_h[2]), size = 3.5) +
  ggplot2::annotate(geom = "text", x=8.5, y=0.4,
                    label=paste('Avg. Bin Length =', round(((mainp$bin_avg[1]*mainp$J[1] + mainp$bin_avg[2]*mainp$J[2])/(mainp$J[1]+mainp$J[2])),1)), size = 3.5) +
  #ggtitle('Main effect') +   
  theme(plot.title = element_text(size=18, lineheight=3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size =14),
        axis.title.y = element_text(size =14),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureJ1a.pdf', width=6, height=4.85)




### Figure J.1b: Ethnic minority victory effects on media tone of placebo groups
## Comparison of main and placebo effects across months after election
outcomes <- c('prop_negative', 'num_negative', 'prop_positive', 'num_positive')
estimates <- vector(mode = 'list', length = 10)
for (i in seq(1,10,1)) {
  
  coef <- c()
  se <- c()
  p_value <- c()
  cil <- c()
  cir <- c()
  mean_control <- c()
  std_effect <- c()
  bw <-  c()
  nl <- c()
  nr <- c()
  Nl <- c()
  Nr <- c()
  
  for (j in outcomes) {
    
    if (i < 3) {
      nnmatch = 3
    } else {
      nnmatch = i
    }
    main <- rdrobust(sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                     sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin], p = 1,
                     kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                     cluster = sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])
    
    main_covar <- rdrobust(sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i), get(j)],
                           sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i), victory_margin],
                           covs = sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i), covars_media, with = FALSE],
                           p = 1,
                           kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                           cluster = sentiment_placebo[(region_name!='Scotland' & as.numeric(as.character(period))<=i), cluster])
    
    # estimates
    coef <- c(coef, main$coef[1,1])
    coef <- c(coef, main_covar$coef[1,1])
    se <- c(se, main$se[3,1])
    se <- c(se, main_covar$se[3,1])
    p_value <- c(p_value, main$pv[3,1])
    p_value <- c(p_value, main_covar$pv[3,1])
    cil <- c(cil, main$ci[3,1])
    cil <- c(cil, main_covar$ci[3,1])
    cir <- c(cir, main$ci[3,2])
    cir <- c(cir, main_covar$ci[3,2])
    mean_control <- c(mean_control, main$beta_p_l[1])
    mean_control <- c(mean_control, main_covar$beta_p_l[1])
    std_effect <- c(std_effect, main$coef[1,1]/(sentiment_placebo[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0) &
                                                                     (region_name!='Scotland') & (as.numeric(as.character(period))<=i)),
                                                                  sd(get(j), na.rm = TRUE)]))
    std_effect <- c(std_effect, main_covar$coef[1,1]/(sentiment_placebo[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0) &
                                                                           (region_name!='Scotland') & (as.numeric(as.character(period))<=i)),
                                                                        sd(get(j), na.rm = TRUE)]))
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
    
  }
  
  estimates[[i]] <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                               std_effect=std_effect,
                               bw=bw,nl=nl,nr=nr,Nl=Nl,Nr=Nr,
                               cov = rep(c('no', 'yes'), length(outcomes)),
                               outcome = rep(outcomes, each=2),
                               month = rep(i, length(coef)))
}  


estimates_placebo <- data.table(dplyr::bind_rows(estimates))
estimates_placebo[, type:='placebo effect']

# load main estimates from Figure 3b
estimates_main <- readRDS('./output/estimates/media_estimates.RDS')
estimates_main[, type:='main effect']

estimates_all <- rbind(estimates_main, estimates_placebo)

ggplot(estimates_all[outcome %in% c('prop_negative') & cov=='yes'],
                   aes(x=month, y=coef, color=type)) + geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, position=position_dodge(0.2)) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  scale_x_continuous(breaks = c(1:10)) +
  scale_color_manual(values=c("#0000FF", '#666666')) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on proportion',
             "of negative mentions about candidate's",
             "ethnic group and placebo groups",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave('./output/figures/figureJ1b.pdf', width=6, height=4.85)


