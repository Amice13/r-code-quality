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
sentiment_paper <- readRDS('./data/media/media_paper_alignment.RDS')
sentiment_paper_id <- readRDS('./data/media/media_paper_ideology.RDS')
sentiment_paper_circ <- readRDS('./data/media/media_paper_circulation.RDS')
covars_media <- readRDS('./data/media/covars_media.RDS')

### Figure K.1a: Effects by newspaper-candidate's party alignment
sentiment_paper[ ,cluster:=paste(election,pcon18cd,sep='_')]
sentiment_paper[ , party_left:=ifelse(party_political_position=='left',1,0)]
sentiment_paper[is.na(young) & region_name!='Scotland', young:=young_share]

outcomes <- c('prop_negative', 'num_negative', 'prop_positive', 'num_positive', 'prop_neutral', 'num_neutral')

estimates <- vector(mode = 'list', length = 10)
for (i in seq(1,10,1)) {
  
  coef <- c()
  se <- c()
  p_value <- c()
  cil <- c()
  cir <- c()
  cil_con <- c()
  cir_con <- c()
  mean_control <- c()
  std_effect <- c()
  bw <-  c()
  nl <- c()
  nr <- c()
  Nl <- c()
  Nr <- c()
  
  for (s in c(0,1)) {
    
    for (j in outcomes) {
      
      if (i < 3) {
        nnmatch = 3
      } else {
        nnmatch = i
      }
      
      main <- rdrobust(sentiment_paper[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                      paper_supports_candidate_wsmin==s & !is.na(first_generation)), get(j)],
                       sentiment_paper[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                      paper_supports_candidate_wsmin==s & !is.na(first_generation)), victory_margin], p = 1,
                       kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                       cluster = sentiment_paper[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                paper_supports_candidate_wsmin==s & !is.na(first_generation)), cluster])
      
      main_covar <- rdrobust(sentiment_paper[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                            paper_supports_candidate_wsmin==s & !is.na(first_generation)), get(j)],
                             sentiment_paper[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                            paper_supports_candidate_wsmin==s & !is.na(first_generation)), victory_margin],
                             covs = sentiment_paper[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                   paper_supports_candidate_wsmin==s & !is.na(first_generation)), covars_media, with = FALSE],
                             p = 1,
                             kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                             cluster = sentiment_paper[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                      paper_supports_candidate_wsmin==s & !is.na(first_generation)), cluster])
      
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
      cil_con <- c(cil_con, main$ci[1,1])
      cil_con <- c(cil_con, main_covar$ci[1,1])
      cir_con <- c(cir_con, main$ci[1,2])
      cir_con <- c(cir_con, main_covar$ci[1,2])
      mean_control <- c(mean_control, main$beta_p_l[1])
      mean_control <- c(mean_control, main_covar$beta_p_l[1])
      std_effect <- c(std_effect, main$coef[1,1]/(sentiment_paper[(victory_margin >= -main$bws[1,1] & victory_margin < 0 &
                                                                                 region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                                 paper_supports_candidate_wsmin==s & !is.na(first_generation)),
                                                                              sd(get(j), na.rm = TRUE)]))
      std_effect <- c(std_effect, main_covar$coef[1,1]/(sentiment_paper[(victory_margin >= -main$bws[1,1] & victory_margin < 0 &
                                                                                       region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                                       paper_supports_candidate_wsmin==s & !is.na(first_generation)),
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
  }
  
  estimates[[i]] <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,
                               cil_con=cil_con,cir_con=cir_con,mean_control=mean_control,
                               std_effect=std_effect,bw=bw,nl=nl,nr=nr,Nl=Nl,Nr=Nr,
                               cov = rep(c('no', 'yes'), length(outcomes)),
                               outcome = rep(outcomes, each=2),
                               aligned = rep(c(0,1), each=length(outcomes)*2),
                               month = rep(i, length(coef)))
}  

estimates_all <- data.table(dplyr::bind_rows(estimates))

aligned.labs <- c(paste("Newspaper doesn't support", "candidate's party", sep='\n'),
                  paste("Newspaper supports", "candidate's party", sep='\n'))
names(aligned.labs) <- c("0", "1")

## Plots with estimates
ggplot(estimates_all[outcome %in% c('prop_positive', 'prop_negative', 'prop_neutral') & cov=='yes'],
                     aes(x=month, y=coef, color=outcome)) + geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(ymin=cil_con, ymax=cir_con), width=.1, position=position_dodge(0.3)) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  scale_x_continuous(breaks = c(1:10)) +
  ylim(-0.4, 0.6) +
  scale_color_manual(values=c("#0000FF", '#666666', '#FF3399'), labels = c("% negative", "% neutral", "% positive")) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on mentions',
             "about candidate's ethnic group",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3)) +
  facet_grid(.~ aligned, labeller = labeller(aligned = aligned.labs))
ggsave(file = './output/figures/figureK1a.pdf', width=6.5, height=4.85)



#### Figure K.1b: Effects by newspaper ideology
sentiment_paper_id[ ,cluster:=paste(election,pcon18cd,sep='_')]
sentiment_paper_id[ , party_left:=ifelse(party_political_position=='left',1,0)]
sentiment_paper_id[is.na(young) & region_name!='Scotland', young:=young_share]
outcomes <- c('prop_negative', 'num_negative', 'prop_positive', 'num_positive', 'prop_neutral', 'num_neutral')

estimates <- vector(mode = 'list', length = 10)
for (i in seq(1,10,1)) {
  
  coef <- c()
  se <- c()
  p_value <- c()
  cil <- c()
  cir <- c()
  cil_con <- c()
  cir_con <- c()
  mean_control <- c()
  std_effect <- c()
  bw <-  c()
  nl <- c()
  nr <- c()
  Nl <- c()
  Nr <- c()
  
  for (s in c(0,1)) {
    
    for (j in outcomes) {
      
      if (i < 3) {
        nnmatch = 3
      } else {
        nnmatch = i
      }
      
      main <- rdrobust(sentiment_paper_id[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                       paper_conservative==s & !is.na(first_generation)), get(j)],
                       sentiment_paper_id[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                       paper_conservative==s & !is.na(first_generation)), victory_margin], p = 1,
                       kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                       cluster = sentiment_paper_id[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                 paper_conservative==s & !is.na(first_generation)), cluster])
      
      main_covar <- rdrobust(sentiment_paper_id[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                             paper_conservative==s & !is.na(first_generation)), get(j)],
                             sentiment_paper_id[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                             paper_conservative==s & !is.na(first_generation)), victory_margin],
                             covs = sentiment_paper_id[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                    paper_conservative==s & !is.na(first_generation)), covars_media, with = FALSE],
                             p = 1,
                             kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                             cluster = sentiment_paper_id[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                       paper_conservative==s & !is.na(first_generation)), cluster])
      
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
      cil_con <- c(cil_con, main$ci[1,1])
      cil_con <- c(cil_con, main_covar$ci[1,1])
      cir_con <- c(cir_con, main$ci[1,2])
      cir_con <- c(cir_con, main_covar$ci[1,2])
      mean_control <- c(mean_control, main$beta_p_l[1])
      mean_control <- c(mean_control, main_covar$beta_p_l[1])
      std_effect <- c(std_effect, main$coef[1,1]/(sentiment_paper_id[(victory_margin >= -main$bws[1,1] & victory_margin < 0 &
                                                                                  region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                                  paper_conservative==s & !is.na(first_generation)),
                                                                               sd(get(j), na.rm = TRUE)]))
      std_effect <- c(std_effect, main_covar$coef[1,1]/(sentiment_paper_id[(victory_margin >= -main$bws[1,1] & victory_margin < 0 &
                                                                                        region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                                        paper_conservative==s & !is.na(first_generation)),
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
  }
  
  estimates[[i]] <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,
                               cil_con=cil_con,cir_con=cir_con,mean_control=mean_control,
                               std_effect=std_effect,bw=bw,nl=nl,nr=nr,Nl=Nl,Nr=Nr,
                               cov = rep(c('no', 'yes'), length(outcomes)),
                               outcome = rep(outcomes, each=2),
                               conservative = rep(c(0,1), each=length(outcomes)*2),
                               month = rep(i, length(coef)))
}  

estimates_all <- data.table(dplyr::bind_rows(estimates))
ideology.labs <- c(paste("Right-wing", "newspaper", sep='\n'),
                   paste("Left-wing", "newspaper", sep='\n'))
names(ideology.labs) <- c("1", "0")

## Plots with estimates
ggplot(estimates_all[outcome %in% c('prop_positive', 'prop_negative', 'prop_neutral') & cov=='yes'],
       aes(x=month, y=coef, color=outcome)) + geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(ymin=cil_con, ymax=cir_con), width=.1, position=position_dodge(0.3)) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  #geom_line(aes(x=month, y=(nl+nr))) +
  scale_x_continuous(breaks = c(1:10)) +
  ylim(-0.4, 0.6) +
  scale_color_manual(values=c("#0000FF", '#666666', '#FF3399'), labels = c("% negative", "% neutral", "% positive")) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on mentions',
             "about candidate's ethnic group",
             sep='\n')) +
  #ggtitle("Effect by newspaper support") +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3)) +
  facet_grid(.~ conservative, labeller = labeller(conservative = ideology.labs))
ggsave('./output/figures/figureK1b.pdf', width=6.5, height=4.85)



### Figure K.1c: Effects by newspaper circulation
sentiment_paper_circ[ ,cluster:=paste(election,pcon18cd,sep='_')]
sentiment_paper_circ[ , party_left:=ifelse(party_political_position=='left',1,0)]
sentiment_paper_circ[is.na(young) & region_name!='Scotland', young:=young_share]

outcomes <- c('prop_negative', 'num_negative', 'prop_positive', 'num_positive', 'prop_neutral', 'num_neutral')

estimates <- vector(mode = 'list', length = 10)
for (i in seq(1,10,1)) {
  
  coef <- c()
  se <- c()
  p_value <- c()
  cil <- c()
  cir <- c()
  cil_con <- c()
  cir_con <- c()
  mean_control <- c()
  std_effect <- c()
  bw <-  c()
  nl <- c()
  nr <- c()
  Nl <- c()
  Nr <- c()
  
  for (s in c(0,1)) {
    
    for (j in outcomes) {
      
      if (i < 3) {
        nnmatch = 3
      } else {
        nnmatch = i
      }
      
      main <- rdrobust(sentiment_paper_circ[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                          high_circulation==s & !is.na(first_generation)), get(j)],
                       sentiment_paper_circ[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                          high_circulation==s & !is.na(first_generation)), victory_margin], p = 1,
                       kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                       cluster = sentiment_paper_circ[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                    high_circulation==s & !is.na(first_generation)), cluster])
      
      main_covar <- rdrobust(sentiment_paper_circ[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                high_circulation==s & !is.na(first_generation)), get(j)],
                             sentiment_paper_circ[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                high_circulation==s & !is.na(first_generation)), victory_margin],
                             covs = sentiment_paper_circ[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                       high_circulation==s & !is.na(first_generation)), covars_media, with = FALSE],
                             p = 1,
                             kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                             cluster = sentiment_paper_circ[(region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                          high_circulation==s & !is.na(first_generation)), cluster])
      
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
      cil_con <- c(cil_con, main$ci[1,1])
      cil_con <- c(cil_con, main_covar$ci[1,1])
      cir_con <- c(cir_con, main$ci[1,2])
      cir_con <- c(cir_con, main_covar$ci[1,2])
      mean_control <- c(mean_control, main$beta_p_l[1])
      mean_control <- c(mean_control, main_covar$beta_p_l[1])
      std_effect <- c(std_effect, main$coef[1,1]/(sentiment_paper_circ[(victory_margin >= -main$bws[1,1] & victory_margin < 0 &
                                                                                     region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                                     high_circulation==s & !is.na(first_generation)),
                                                                                  sd(get(j), na.rm = TRUE)]))
      std_effect <- c(std_effect, main_covar$coef[1,1]/(sentiment_paper_circ[(victory_margin >= -main$bws[1,1] & victory_margin < 0 &
                                                                                           region_name!='Scotland' & as.numeric(as.character(period))<=i &
                                                                                           high_circulation==s & !is.na(first_generation)),
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
  }
  
  estimates[[i]] <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,
                               cil_con=cil_con,cir_con=cir_con,mean_control=mean_control,
                               std_effect=std_effect,bw=bw,nl=nl,nr=nr,Nl=Nl,Nr=Nr,
                               cov = rep(c('no', 'yes'), length(outcomes)),
                               outcome = rep(outcomes, each=2),
                               high_circulation = rep(c(0,1), each=length(outcomes)*2),
                               month = rep(i, length(coef)))
}  

estimates_all <- data.table(dplyr::bind_rows(estimates))

circulation.labs <- c(paste("Paper circulation", "> 25,000", sep='\n'),
                      paste("Paper circulation", "< 25,000", sep='\n'))
names(circulation.labs) <- c("1", "0")

## Plots with estimates
ggplot(estimates_all[outcome %in% c('prop_positive', 'prop_negative', 'prop_neutral') & cov=='yes'],
       aes(x=month, y=coef, color=outcome)) + geom_point(position=position_dodge(0.3)) +
  geom_errorbar(aes(ymin=cil_con, ymax=cir_con), width=.1, position=position_dodge(0.3)) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  #geom_line(aes(x=month, y=(nl+nr))) +
  scale_x_continuous(breaks = c(1:10)) +
  ylim(-0.4, 0.6) +
  scale_color_manual(values=c("#0000FF", '#666666', '#FF3399'), labels = c("% negative", "% neutral", "% positive")) +
  xlab('Number of months after election') +
  ylab(paste('Minority victory effects on mentions',
             "about candidate's ethnic group",
             sep='\n')) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3)) +
  facet_grid(.~ high_circulation, labeller = labeller(high_circulation = circulation.labs))
ggsave('./output/figures/figureK1c.pdf', width=6.5, height=4.85)
