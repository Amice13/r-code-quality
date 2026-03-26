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
load('./data/crime/hate_crime.RDS')
covars_crime <- readRDS('./data/crime/covars_crime.RDS')
demographic_change <- readRDS('./data/demographic/demographic_change.RDS')

### Figure G10a: Subgroup effects on hate crime, by change in local demographic conditions
hate_crimes_w_at_3m <- merge(hate_crimes_w_at_3m,
                             demographic_change,
                             by.x = 'PCON17CD', by.y = 'geogcode')

# define variables change in migrant population, unemployment
hate_crimes_w_at_3m[, migrant_influx:=((migrant_share_2011-migrant_share_2001)/migrant_share_2001)]
hate_crimes_w_at_3m[, economic_downturn:=(economically_unemployed_2011-(economically_unemployed_2001*100))/(economically_unemployed_2001*100)]
hate_crimes_w_at_3m[, high_migrant_influx:=ifelse(migrant_influx>median(migrant_influx, na.rm = TRUE),1,0)]
hate_crimes_w_at_3m[, high_economic_downturn:=ifelse(economic_downturn>median(economic_downturn, na.rm = TRUE),1,0)]

# compute estimates
out_hmi <- rdrobust(hate_crimes_w_at_3m[high_migrant_influx==1, crime_rate],
                    hate_crimes_w_at_3m[high_migrant_influx==1, victory_margin],
                    kernel = "triangular", p = 1,
                    nnmatch = 4,
                    cluster = hate_crimes_w_at_3m[high_migrant_influx==1, cluster]) 

data_hmi <- cbind('Migrant influx', 'high',out_hmi$coef[1], out_hmi$ci[3,1], out_hmi$ci[3,2], out_hmi$se[1])

out_lmi <- rdrobust(hate_crimes_w_at_3m[high_migrant_influx==0, crime_rate],
                    hate_crimes_w_at_3m[high_migrant_influx==0, victory_margin],
                    kernel = "triangular", p = 1,
                    nnmatch = 4,
                    cluster = hate_crimes_w_at_3m[high_migrant_influx==0, cluster]) 

data_lmi <- cbind('Migrant influx', 'low',out_lmi$coef[1], out_lmi$ci[3,1], out_lmi$ci[3,2], out_lmi$se[1])

out_hed <- rdrobust(hate_crimes_w_at_3m[high_economic_downturn==1, crime_rate],
                    hate_crimes_w_at_3m[high_economic_downturn==1, victory_margin],
                    kernel = "triangular", p = 1,
                    nnmatch = 4,
                    cluster = hate_crimes_w_at_3m[high_economic_downturn==1, cluster]) 

data_hed <- cbind('Economic downturn', 'high',out_hed$coef[1], out_hed$ci[3,1], out_hed$ci[3,2], out_hed$se[1])

out_led <- rdrobust(hate_crimes_w_at_3m[high_economic_downturn==0, crime_rate],
                    hate_crimes_w_at_3m[high_economic_downturn==0, victory_margin],
                    kernel = "triangular", p = 1,
                    nnmatch = 4,
                    cluster = hate_crimes_w_at_3m[high_economic_downturn==0, cluster]) 

data_led <- cbind('Economic downturn', 'low',out_led$coef[1], out_led$ci[3,1], out_led$ci[3,2], out_led$se[1])

data_interactions <- data.frame(rbind(data_hmi, data_lmi,
                                      data_hed, data_led))
colnames(data_interactions) <- c('group', 'type', 'coef', 'ci_l', 'ci_u', 'se')

ggplot(data_interactions,
       aes(x=group, y=as.numeric(coef), color=type)) + geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(ymin=as.numeric(ci_l), ymax=as.numeric(ci_u)), width=.1, position=position_dodge(0.3)) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  xlab(NULL) +
  ylab(paste('Minority victory effects on',
             "hate crimes per 1000 residents",
             '3 months after general election',
             sep='\n')) +
  scale_color_grey(end = 0.6) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figureG10a.pdf', width=6, height=4.85)


## diff in coeffs t stat
diff_means <- function(coef1, coef2, se1, se2) {
  (coef1 - coef2)/sqrt(se1^2 + se2^2)
}
# in-text nubers
diff_mig <- round(diff_means(out_hmi$coef[1], out_lmi$coef[1], out_hmi$se[1], out_lmi$se[1]),2)
diff_mig

diff_econ <- round(diff_means(out_hed$coef[1], out_led$coef[1], out_hed$se[1], out_led$se[1]),2)
diff_econ


### Figure G10b: Subgroup effects on hate crime, controlling for history of an ethnic minority MP
## compare effects across main specification and specification controlling for history of BME MP
covars <- c(covars_crime, 'bme_mp_history')

dataset <- list(hate_crimes_w_at_1m, hate_crimes_w_at_2m, hate_crimes_w_at_3m, hate_crimes_w_at_4m,
                hate_crimes_w_at_5m, hate_crimes_w_at_6m, hate_crimes_w_at_7m, hate_crimes_w_at_8m,
                hate_crimes_w_at_9m)

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
                         covs = data[, covars, with = FALSE],
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

estimates <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                        std_effect=std_effect,bw=bw,n=nl+nr,N=Nl+Nr,
                        cov = rep(c('no', 'yes'), length(coef)/2),
                        month = month)

estimates <- data.table(estimates) 
estimates[,type:='controlling for minority MP history']

# load estimates from main model
estimates_main <- readRDS(file = './output/estimates/crime_estimates.RDS')
estimates_main[,type:='main']

estimates_all <- rbind(estimates_main, estimates, fill=TRUE)
estimates_all$type <-  factor(estimates_all$type, levels = c('main', 'controlling for minority MP history'))

ggplot(estimates_all[cov=='yes'],
       aes(x=month, y=coef, color=type)) + geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, position=position_dodge(0.2)) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  scale_x_continuous(breaks = c(1:10)) +
  scale_color_manual(values=c("#0000FF", "#2f3132")) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on',
             "hate crimes per 1000 residents",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figureG10b.pdf', width=6, height=4.85)
