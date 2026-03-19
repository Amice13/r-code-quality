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
sentiment <- readRDS('./data/media/media.RDS')
sentiment_no_crime <- readRDS('./data/media/media_no_crime.RDS')
covars_media <- readRDS('./data/media/covars_media.RDS')
sentiment[,labour:=ifelse((party_name=='Labour' | party_name=='Labour and Co-operative'),1,0)]

### Figure J.5a: Controlling for candidate's political party
covars <- c(covars_media, 'labour')

outcomes <- c('prop_negative')
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
    main <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), get(j)],
                     sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), victory_margin], p = 1,
                     kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                     cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])
    
    main_covar <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), get(j)],
                           sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), victory_margin],
                           covs = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), c(covars), with = FALSE],
                           p = 1,
                           kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                           cluster = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])
    
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
    std_effect <- c(std_effect, main$coef[1,1]/(sentiment[(victory_margin >= -main$bws[1,1] & victory_margin < 0 &
                                                             region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                             !is.na(first_generation)),
                                                          sd(get(j), na.rm = TRUE)]))
    std_effect <- c(std_effect, main_covar$coef[1,1]/(sentiment[(victory_margin >= -main_covar$bws[1,1] & victory_margin < 0 &
                                                                   region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                   !is.na(first_generation)),
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

estimates_party <- data.table(dplyr::bind_rows(estimates))
estimates_party[,type:='controlling for party affiliation']

## plot comparing effects with/without controlling for candidate's party
# load main estimates computed in Figure 3b
estimates_main <- readRDS( './output/estimates/media_estimates.RDS')
estimates_main[,type:='main']

estimates_all <- rbind(estimates_main, estimates_party)
estimates_all$type <-  factor(estimates_all$type, levels = c('main', 'controlling for party affiliation'))

ggplot(estimates_all[cov=='yes' & outcome=='prop_negative'],
       aes(x=month, y=coef, color=type)) + geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, position=position_dodge(0.2)) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  scale_x_continuous(breaks = c(1:10)) +
  scale_color_manual(values=c("#0000FF", "#2f3132")) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on',
             "proportion of negative mentions",
             "about a candidate's ethnic group",
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
ggsave(file = './output/figures/figureJ5a.pdf', width=6, height=4.85)



#### Figure J.5b: Ethnic minority victory effects on media tone excluding mentions of hate crime
sentiment_no_crime[ ,cluster:=paste(election,pcon18cd,sep='_')]
sentiment_no_crime[ , party_left:=ifelse(party_political_position=='left',1,0)]
sentiment_no_crime[is.na(young) & region_name!='Scotland', young:=young_share]
sentiment_no_crime[is.na(grade_c1) & region_name!='Scotland', grade_c1:=gradea_c1]

# Table with estimates
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
    main <- rdrobust(sentiment_no_crime[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), get(j)],
                     sentiment_no_crime[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), victory_margin], p = 1,
                     kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                     cluster = sentiment_no_crime[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])
    
    main_covar <- rdrobust(sentiment_no_crime[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), get(j)],
                           sentiment_no_crime[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), victory_margin],
                           covs = sentiment_no_crime[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), covars_media, with = FALSE],
                           p = 1,
                           kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                           cluster = sentiment_no_crime[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])
    
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
    std_effect <- c(std_effect, main$coef[1,1]/(sentiment_no_crime[(victory_margin >= -main$bws[1,1] & victory_margin < 0 &
                                                             region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                             !is.na(first_generation)),
                                                          sd(get(j), na.rm = TRUE)]))
    std_effect <- c(std_effect, main_covar$coef[1,1]/(sentiment_no_crime[(victory_margin >= -main_covar$bws[1,1] & victory_margin < 0 &
                                                                   region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                   !is.na(first_generation)),
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

estimates_no_crime <- data.table(dplyr::bind_rows(estimates))
estimates_no_crime[,type:='w/o crime articles effect']

## plot main and main w/o crime sentiment effects by month
# load main estimates computed in Figure 3b
estimates_main <-  readRDS('./output/estimates/media_estimates.RDS')
estimates_main[,type:='main effect']

estimates_all <- rbind(estimates_main, estimates_no_crime)

ggplot(estimates_all[outcome %in% c('prop_negative') & cov=='yes'],
       aes(x=month, y=coef, color=type)) + geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, position=position_dodge(0.2)) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  scale_x_continuous(breaks = c(1:10)) +
  scale_color_manual(values=c("#0000FF", '#666666')) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on proportion',
             "of negative mentions about",
             "candidate's ethnic group",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figureJ5b.pdf', width=6, height=4.85)
