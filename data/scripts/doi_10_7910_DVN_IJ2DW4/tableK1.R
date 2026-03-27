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
library(xtable)

## load data
sentiment_3_categories <- readRDS('./data/media/media_3_classification_categories.RDS')
covars_media <- readRDS('./data/media/covars_media.RDS')

### Table K.1: Minority victory effects across media valence categories
## Estimate RD effects on negative, neutral, positive mentions and then take differences in estimates
sentiment_3_categories[ ,cluster:=paste(election,pcon18cd,sep='_')]
sentiment_3_categories[ , party_left:=ifelse(party_political_position=='left',1,0)]
sentiment_3_categories[is.na(young) & region_name!='Scotland', young:=young_share]

outcomes <- c('prop_negative', 'prop_neutral', 'prop_positive',
              'num_negative', 'num_neutral', 'num_positive')

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


# are differences across valence categories statistically significant?
z_neg_pos <- vector(mode = 'list', length=estimates_all[,max(month)])
z_neg_neu <- vector(mode = 'list', length=estimates_all[,max(month)])
z_pos_neu <- vector(mode = 'list', length=estimates_all[,max(month)])

for (i in 1:estimates_all[,max(month)]) {
  z_neg_pos[[i]] <- dcast(melt(estimates_all[cov=='yes', .(coef, se, outcome, month)],
                               id.vars =c('outcome', 'month')), . ~ variable + outcome + month)[,
                                                                                                ((get(paste0('coef_prop_negative_',i))-get(paste0('coef_prop_positive_',i))) /
                                                                                                   (sqrt((get(paste0('se_prop_negative_',i)))^2 + (get(paste0('se_prop_positive_',i)))^2)))]
  
  
  z_neg_neu[[i]] <- dcast(melt(estimates_all[cov=='yes', .(coef, se, outcome, month)],
                               id.vars =c('outcome', 'month')), . ~ variable + outcome + month)[,
                                                                                                ((get(paste0('coef_prop_negative_',i))-get(paste0('coef_prop_neutral_',i))) /
                                                                                                   (sqrt((get(paste0('se_prop_negative_',i)))^2 + (get(paste0('se_prop_neutral_',i)))^2)))]
  
  
  z_pos_neu[[i]] <- dcast(melt(estimates_all[cov=='yes', .(coef, se, outcome, month)],
                               id.vars =c('outcome', 'month')), . ~ variable + outcome + month)[,
                                                                                                ((get(paste0('coef_prop_positive_',i))-get(paste0('coef_prop_neutral_',i))) /
                                                                                                   (sqrt((get(paste0('se_prop_positive_',i)))^2 + (get(paste0('se_prop_neutral_',i)))^2)))]
  
}

z_table <- data.frame(month = seq(1, estimates_all[,max(month)], by=1),
                      'neg-pos' = round(unlist(rbind(z_neg_pos)),2),
                      'neg-neu' = round(unlist(rbind(z_neg_neu)),2),
                      'pos-neu' = round(unlist(rbind(z_pos_neu)),2))

names(z_table) <- c('month', '(negative - positive)', '(negative - neutral)', '(positive - neutral)')

# create footnotes for table
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(z_table))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Notes: Values indicate the t-statistic of the difference between the RD estimates of the effects of a minority win on the proportion of negative, positive, and neutral mentions about a candidate's ethnic group in the media. Values larger than the critical value of 1.96 are statistically significant.}\n", sep = ""))

# print table
print(xtable(z_table,
             align = "lcccc",       
             caption = "Minority victory effects across media valence categories",
             label = "table:table_k1",
             digits = c(0,0,2,2,2)),
      include.rownames=FALSE,
      caption.placement = "top",
      add.to.row = comment,
      timestamp = NULL,
      file='./output/tables/tableK1.tex')
