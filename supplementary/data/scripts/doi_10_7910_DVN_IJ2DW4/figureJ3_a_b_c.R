rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

# *NOTE*: install RATest version 0.1.9 as indicated below
# *NOTE*: 'gridExtra', 'quantreg' need to be installed before installing RATest
# install.packages('gridExtra')
# install.packages('quantreg')
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/RATest/RATest_0.1.9.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(tidyr)
library(dplyr)
library(rdrobust)
library(RATest)
library(ggplot2)


## load data
sentiment <- readRDS('./data/media/media.RDS')
sentiment_before <- readRDS('./data/media/media_before_election.RDS')
covars_media <- readRDS('./data/media/covars_media.RDS')


### Figure J.3: Continuity of predetermined variables around the cutoff
## Figure a: Continuity of means using local linear regression
baseline <- c('incumbent',
              'female',
              'first_generation',
              'party_left',
              'ethnicity_minority',
              'religion_minority',
              'migrant_share',
              'sub_region_prop',
              'density',
              'young',
              'single_share',
              'deprivation1',
              'deprivation2',
              'deprivation3',
              'deprivation4',
              'grade_ab',
              'grade_c1',
              'grade_c2',
              'grade_de',
              'qualification_level1',
              'qualification_level2',
              'qualification_level3',
              'qualification_level4',
              'economically_inactive',
              'economically_student',
              'economically_unemployed',
              'economically_employed',
              'tenure_rent_free',
              'tenure_owned',
              'tenure_social_rented',
              'tenure_private_rented',
              'ukip')

baseline_pv <- c()

for (i in 1:length(baseline)) {
  rdout <- rdrobust(sentiment[(region_name!='Scotland' & as.numeric(as.character(period))==1), get(baseline[i])],
                    sentiment[(region_name!='Scotland' & as.numeric(as.character(period))==1), victory_margin],
                    kernel = "triangular", p = 1, bwselect = "mserd")
  
  baseline_pv <- c(baseline_pv, rdout$pv[3])
  
}

baseline_labels <- c('Candidate: incumbent',
                     'Candidate: female',
                     'Candidate: 1st gen',
                     'Candidate: party left',
                     '% Ethnic minority',
                     '% Non-dominant religion',
                     '% Immigrant',
                     'Candidate: % ethnic group',
                     'Population density',
                     '% Young',
                     '% Single' ,
                     '% Deprivation level 1',
                     '% Deprivation level 2',
                     '% Deprivation level 3',
                     '% Deprivation level 4',
                     '% Social grade ab',
                     '% Social gradea c1',
                     '% Social grade c2',
                     '% Social grade de',
                     '% Level 1 qualifications' ,
                     '% Level 2 qualifications' ,
                     '% Level 3 qualifications' ,
                     '% Level 4+ qualifications' ,
                     '% Economically inactive',
                     '% Economically active: students' ,
                     '% Economically active: unemployed',
                     '% Economically active: employed',
                     '% Tenure: rent free',
                     '% Tenure: owned',
                     '% Tenure: social rented',
                     '% Tenure: private rented',
                     '% Vote UKIP')
data_graph <- data.frame('baseline'=baseline_labels, 'p_values'=baseline_pv)
data_graph <- data.table(data_graph)
data_graph <- data_graph[order(p_values)]

ggplot(data_graph) +
  aes(x=p_values, y=reorder(baseline,-p_values)) +
  geom_point(colour='#0000FF', size=1.5) +
  geom_vline(xintercept = 0.05,  linetype='longdash', colour='#666666') +
  ggtitle('Continuity of means predetermined variable') +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=9),
        axis.title.x = element_text(size =12)) +
  xlab('P-value of test for continuity of means around cutoff') +
  ylab(NULL)
ggsave('./output/figures/figureJ3a.pdf', width=6.2, height=4.85)



## Figure b: Continuity of distribution using asymptotic permutation test for observations near the cutoff
set.seed(42)
permtest <- RDperm(W = baseline,
                   z = "victory_margin",
                   data = sentiment[(region_name!='Scotland' & as.numeric(as.character(period))==1)])

baseline_pv <- unname(permtest$results[,2])
data_graph <- data.frame('baseline'=c(baseline_labels,'joint_test'), 'p_values'=baseline_pv)
data_graph <- data.table(data_graph) # cannot compute stat for candidate: 1st gen
data_graph <- data_graph[!(baseline %in% c('Candidate: 1st gen', 'joint_test'))]
data_graph <- data_graph[order(p_values)]

ggplot(data_graph) +
  aes(x=p_values, y=reorder(baseline,-p_values)) +
  geom_point(colour='#0000FF', size=2) +
  geom_vline(aes(xintercept=0.05),
             linetype='longdash', colour='#666666') +
  ggtitle('Continuity of distribution predetermined variable') +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=9),
        axis.title.x = element_text(size =12)) +
  xlab('P-value of test for continuity of distribution around cutoff') +
  ylab(NULL)
ggsave('./output/figures/figureJ3b.pdf', width=6.2, height=4.85)



### Figure c: Ethnic minority victory effects on media tone before and after the election
sentiment_before[ ,cluster:=paste(election,pcon18cd,sep='_')]
sentiment_before[ , party_left:=ifelse(party_political_position=='left',1,0)]
sentiment_before[is.na(young) & region_name!='Scotland', young:=young_share]
sentiment_before[is.na(grade_c1) & region_name!='Scotland', grade_c1:=gradea_c1]

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
    main <- rdrobust(sentiment_before[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), get(j)],
                     sentiment_before[(region_name!='Scotland' & as.numeric(as.character(period))<=i) & !is.na(first_generation), victory_margin], p = 1,
                     kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                     cluster = sentiment_before[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])
    
    main_covar <- rdrobust(sentiment_before[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), get(j)],
                           sentiment_before[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), victory_margin],
                           covs = sentiment_before[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), covars_media, with = FALSE],
                           p = 1,
                           kernel = "triangular", bwselect = "mserd", nnmatch = nnmatch,
                           cluster = sentiment_before[(region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)), cluster])
    
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
    std_effect <- c(std_effect, main$coef[1,1]/(sentiment_before[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0) &
                                                                          region_name!='Scotland' & as.numeric(as.character(period))<=i & !is.na(first_generation)),
                                                                       sd(get(j), na.rm = TRUE)]))
    std_effect <- c(std_effect, main_covar$coef[1,1]/(sentiment_before[((victory_margin >= -main$bws[1,1]) & (victory_margin < 0) &
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

estimates_before <- data.table(dplyr::bind_rows(estimates))


## plot effect before/after
estimates_before[, month_rev:=month*-1]

# load estimates computed in Figure 3b
estimates_after <- readRDS('./output/estimates/media_estimates.RDS')
estimates_after[,month_rev:=month]
estimates_all <- rbind(estimates_before,estimates_after)

ggplot(estimates_all[outcome %in% c('prop_negative') & cov=='yes' & month_rev>=-7 & month_rev<=4],
       aes(x=month_rev, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=0.1) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  geom_vline(xintercept=0, linetype='longdash', size=0.2) +
  scale_x_continuous(breaks = c(-7:4), labels = c(seq(-7,4,1))) +
  scale_color_manual(values=c("#0000FF")) +
  xlab('Number of months relative to election') +
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
ggsave('./output/figures/figureJ3c.pdf', width=6, height=4.85)

