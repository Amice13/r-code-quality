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


## load data
sentiment_3_categories <- readRDS('./data/media/media_3_classification_categories.RDS')
covars_media <- readRDS('./data/media/covars_media.RDS')

### Figure 4a
## compute RD estimates 1-10 months from election
outcomes <- c('num_total', 'prop_negative', 'prop_neutral', 'prop_positive')
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
    
    main <- rdrobust(sentiment_3_categories[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), get(j)],
                     sentiment_3_categories[(region_name!='Scotland' & as.numeric(as.character(period))<=i) & !is.na(first_generation), victory_margin], p = 1,
                     kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                     cluster = sentiment_3_categories[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])
    
    main_covar <- rdrobust(sentiment_3_categories[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), get(j)],
                           sentiment_3_categories[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), victory_margin],
                           covs = sentiment_3_categories[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), covars_media, with = FALSE],
                           p = 1,
                           kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                           cluster = sentiment_3_categories[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])
    
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
    std_effect <- c(std_effect, main$coef[1,1]/(sentiment_3_categories[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0) &
                                                                          region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)),
                                                                       sd(get(j), na.rm = TRUE)]))
    std_effect <- c(std_effect, main_covar$coef[1,1]/(sentiment_3_categories[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0) &
                                                                                region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)),
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
                               std_effect=std_effect,bw=bw,nl=nl,nr=nr,Nl=Nl,Nr=Nr,
                               cov = rep(c('no', 'yes'), length(outcomes)),
                               outcome = rep(outcomes, each=2),
                               month = rep(i, length(coef)))
}  

estimates_all <- data.table(dplyr::bind_rows(estimates))

# plot estimates 1-10 months from election
ggplot(estimates_all[outcome %in% c('num_total') & cov=='yes'],
       aes(x=month, y=coef)) + geom_point(color = '#666666') +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, color = '#666666') +
  geom_hline(yintercept=0, linetype='longdash', linewidth=0.2) +
  scale_x_continuous(breaks = c(1:10)) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on number of',
             "mentions about a candidate's ethnic group",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figure4a.pdf', width=6, height=4.85)

# in-text numbers
estimates_all[month==3 & outcome=='num_total' & cov=='yes', round(coef,1)]
estimates_all[month==3 & outcome=='num_total' & cov=='yes', round(p_value,3)] # statistically significant at 10% level

estimates_all[month==3 & outcome=='prop_negative' & cov=='yes', round(std_effect,2)]
estimates_all[month==3 & outcome=='prop_negative' & cov=='yes', round(p_value,3)] # statistically significant

estimates_all[month==3 & outcome=='prop_positive' & cov=='yes', round(std_effect,2)]
estimates_all[month==3 & outcome=='prop_positive' & cov=='yes', round(p_value,3)] # statistically significant

estimates_all[month==3 & outcome=='prop_neutral' & cov=='yes', round(std_effect,2)]
estimates_all[month==3 & outcome=='prop_neutral' & cov=='yes', round(p_value,3)] # not statistically significant


### Figure 4b
ggplot(estimates_all[outcome %in% c('prop_negative', 'prop_neutral', 'prop_positive') & cov=='yes'],
       aes(x=month, y=coef, color=outcome)) + geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, position=position_dodge(0.3)) +
  geom_hline(yintercept=0, linetype='longdash', linewidth=0.2) +
  scale_x_continuous(breaks = c(1:10)) +
  scale_color_manual(values=c("#0000FF", '#666666', '#FF3399'), labels = c("% negative", "% neutral", "% positive")) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on valence of',
             "mentions about a candidate's ethnic group",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave('./output/figures/figure4b.pdf', width=6, height=4.85)
