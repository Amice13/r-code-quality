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

### Figure G12a: Subgroup effects on hate crime by party ideology
hate_crimes_w_at_3m[, party_left:=ifelse(party_political_position=='left',1,0)]
covars <- c(covars_crime[c(1:8)], 'religion_minority') 

out_left <- rdrobust(hate_crimes_w_at_3m[party_left==1, crime_rate],
                     hate_crimes_w_at_3m[party_left==1, victory_margin],
                     covs = hate_crimes_w_at_3m[party_left==1, covars, with=F],
                     kernel = "triangular", p = 1,
                     nnmatch = 4,
                     cluster = hate_crimes_w_at_3m[party_left==1, cluster]) 

data_left <- cbind.data.frame(out_left$coef[1], out_left$ci[3,1], out_left$ci[3,2], out_left$se[1], paste('Left-leaning', 'party', sep='\n'))
colnames(data_left) <- c('coef', 'ci_l', 'ci_u', 'se', 'party')

out_right <- rdrobust(hate_crimes_w_at_3m[party_left==0, crime_rate],
                      hate_crimes_w_at_3m[party_left==0, victory_margin],
                      covs = hate_crimes_w_at_3m[party_left==0, covars, with=F],
                      kernel = "triangular", p = 1,
                      nnmatch = 4,
                      cluster = hate_crimes_w_at_3m[party_left==0, cluster]) 

data_right <- cbind.data.frame(out_right$coef[1], out_right$ci[3,1], out_right$ci[3,2], out_right$se[1],paste('Right-leaning', 'party', sep='\n'))
colnames(data_right) <- c('coef', 'ci_l', 'ci_u', 'se', 'party')

data_party <- rbind(data_left, data_right)

ggplot(data_party,
       aes(x=party, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u), width=.1) +
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
ggsave(file = './output/figures/figureG12a.pdf', width=6, height=4.85)



#### Figure G12b: White Labour victory effects on hate crime
rm(list = ls())
## load data
load('./data/crime/hate_crime_white_candidates.RDS')
covars_crime <- readRDS('./data/crime/covars_crime.RDS')

# plot effects by month
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

estimates <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                        std_effect=std_effect,bw=bw,n=nl+nr,N=Nl+Nr,
                        cov = rep(c('no', 'yes'), length(coef)/2),
                        month = month)
estimates <- data.table(estimates)

ggplot(estimates[cov=='yes'],
       aes(x=month, y=coef)) + geom_point(color = "#2f3132") +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, color = "#2f3132") +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  scale_x_continuous(breaks = c(1:9)) +
  xlab('Number of months after election') +
  ylab(paste('White Labour victory effects on',
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
ggsave(file = './output/figures/figureG12b.pdf', width=6, height=4.85)


## in-text numbers
estimates[month==3 & cov=='yes', .(coef, p_value)]
