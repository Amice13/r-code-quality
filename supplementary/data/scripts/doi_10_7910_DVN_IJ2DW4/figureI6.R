rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(rdrobust)
library(ggplot2)

## load data
load(file='./data/attitudes/attitudes.rds')
bes_rdd <- bes_rdd[grepl('^(E|W|S)', ons_id) & white==1]
covars_attitudes <- readRDS('./data/attitudes/covars_attitudes.RDS')

## Robustness including Scotland
### Figure I.6: Ethnic minority victory effects on attitudes toward immigrants (in England, Scotland and Wales)
outcomes <- c('immigrants_economy', 'j05', 'attitudes_migrants')
y_labels <- c('Economy', 'Entry', 'Index')
covars <- setdiff(covars_attitudes, c( "migrant_share", "ukip", "deprivation4"))
bes_rdd_complete <- bes_rdd[complete.cases(bes_rdd[, covars, with = FALSE])]

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

for (i in outcomes) {
  
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
                         covs = bes_rdd[, covars, with=F],
                         kernel = "triangular", p = 1, bwselect = "mserd",
                         cluster = bes_rdd$cluster)
  
  
  coef <- c(coef, main$coef[1,1])
  coef <- c(coef, main_complete$coef[1,1])
  coef <- c(coef, main_covar$coef[1,1])
  se <- c(se, main$se[1,1])
  se <- c(se, main_complete$se[1,1])
  se <- c(se, main_covar$se[1,1])
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

estimates_all <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                            std_effect=std_effect, std_cil=std_cil, std_cir=std_cir,
                            bw=bw, n=nl+nr, N=Nl+Nr,
                            outcome = rep(y_labels,each=3),
                            cov = rep(c('no','no','yes'),3),
                            sample = rep(c('full', 'complete cases', 'complete cases'),3))
estimates_all <- data.table(estimates_all) 

ggplot(estimates_all[cov=='yes' & sample=='complete cases'],
       aes(y=outcome, x=std_effect)) + geom_point(color = "#2f3132") +
  geom_errorbar(aes(xmin=std_cil, xmax=std_cir), width=.1, color = "#2f3132") +
  geom_vline(xintercept=0, linetype='longdash', linewidth=0.2) +
  ylab(NULL) +
  xlab(paste("Minority victory effects on",
             "inclusionary attitudes (std dev)",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figureI6.pdf', width=6, height=4.85)

