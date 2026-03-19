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
library(rdrobust)
library(RATest)
library(dplyr)
library(tidyr)
library(ggplot2)

## load data
load('./data/crime/hate_crime_before_election.RDS')
covars_crime <- readRDS('./data/crime/covars_crime.RDS')

### Figure G2a: Ethnic minority victory effects before and after general election
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

estimates_before <- data.frame(coef=coef,se=se,p_value=p_value,cil=cil,cir=cir,mean_control=mean_control,
                            std_effect=std_effect, std_cil=std_cil, std_cir=std_cir,
                            bw=bw, n=nl+nr, N=Nl+Nr,
                            cov = rep(c('no', 'yes'), length(coef)/2),
                            month = month,
                            month_rev = month*-1)
estimates_before <- data.table(estimates_before) 

#### plot
estimates_after <- readRDS(file = './output/estimates/crime_estimates.RDS')
estimates_after[ , month_rev:=month]
estimates <- rbind(estimates_before, estimates_after)

ggplot(estimates[cov=='yes' & month_rev>=-6 & month_rev<=3],
       aes(x=month_rev, y=coef)) + geom_point(color = "#2f3132") +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, color = "#2f3132") +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  geom_vline(xintercept=0, linetype='longdash', size=0.2) +
  scale_x_continuous(breaks = c(-6:3), labels = c(seq(-6,3,1))) +
  xlab('Number of months relative to election') +
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
ggsave(file = './output/figures/figureG2a.pdf', width=6, height=4.85)





### Figure G2b: Continuity of means of predetermined covariates using local linear regression
## load data
load('./data/crime/hate_crime.RDS')
hate_crimes_unique <- unique(hate_crimes_w_at_1m, by=c('PCON17CD', 'election'))
hate_crimes_unique[, party_left:=ifelse(party_political_position=='left',1,0)]

baseline <- c('incumbent', 'female', 'party_left',
              'ethnicity_minority',
              'religion_minority',
              'density',
              'young',
              'Single (never married or never registered a same-sex civil partnership)_%' ,
              'deprivation1', 'deprivation2', 'deprivation3', 'deprivation4',
              'grade_ab', 'gradea_c1' ,'grade_c2', 'grade_de',
              'Highest level of qualification: Level 1 qualifications_%' ,
              'Highest level of qualification: Level 2 qualifications_%' ,
              'Highest level of qualification: Level 3 qualifications_%' ,
              'Highest level of qualification: Level 4 qualifications and above_%' ,
              'Economically Inactive_%' , 'Economically active: Full-time student_%' ,
              'Economically active: In employment_%' , 'Economically active: Unemployed_%' ,
              'Living rent free_%' , 'Owned_%' , 'Private rented_%' , 'Social rented_%' ,
              'No people in household have English as a main language (English or Welsh in Wales)_%',
              'At least one but not all people aged 16 and over in household have English as a main language (English or Welsh in Wales)_%',
              'No people aged 16 and over in household but at least one person aged 3 to 15 has English as a main language (English or Welsh in Wales)_%', 
              'Other EU_%' , 'Other countries_%' ,
              'arrived_before1960', 'arrived_1960_1990' , 'arrived_1990_2011',
              'far_right2010')

# adjust for multiple testing
# probability of falsely rejecting at least one null ~ 85% (on average expect 2 to be statistically significant)
(1- (0.95)^length(baseline))
# with average number of false rejections (~ 2)
0.05*length(baseline)

baseline_pv <- c()
bandwidth <- c()

for (i in baseline) {
  rdout <- rdrobust(hate_crimes_unique[[i]],
                    hate_crimes_unique$victory_margin, kernel = "triangular", p = 1, bwselect = "mserd")
  
  baseline_pv <- c(baseline_pv, rdout$pv[3])
  bandwidth <- c(bandwidth, rdout$bws[1,1])
  
}

baseline_labels <- c('Incumbent candidate', 'Female candidate', 'Left party candidate',
                     '% Ethnic minority',
                     '% Non-dominant religion',
                     'Population density',
                     '% Young',
                     '% Single' ,
                     '% Deprivation level 1', '% Deprivation level 2', '% Deprivation level 3', '% Deprivation level 4',
                     '% Social grade ab', '% Social gradea c1' ,'% Social grade c2', '% Social grade de',
                     '% Level 1 qualifications' ,
                     '% Level 2 qualifications' ,
                     '% Level 3 qualifications' ,
                     '% Level 4+ qualifications' ,
                     '% Economically inactive',
                     '% Economically active: students' ,
                     '% Economically active: employed',
                     '% Economically active: unemployed',
                     '% Tenure: rent free' , '% Tenure: owned' , '% Tenure: private rented' , '% Tenure: social rented',
                     '% English main language: none',
                     '% English main language: one > 16',
                     '% English main language: one < 16', 
                     '% Immigrants: EU' , '% Immigrants: non-EU' ,
                     '% Immigrant arrival < 1960', '% Immigrant arrival 1960-1990' , '% Immigrant arrival 1990-2011',
                     '% Vote far-right 2010')
data_graph <- data.frame('baseline'=baseline_labels, 'p_values'=baseline_pv,
                         'bandwidth'=bandwidth)
data_graph <- data.table(data_graph)
data_graph <- data_graph[order(p_values)]

# Benjamini-Hochberg 1995 procedure to control the false discovery rate,
#the proportion of false discoveries among discoveries
data_graph[,rank:=as.numeric(rownames(data_graph))]
data_graph[,cv:=(rank/nrow(data_graph))*0.05]

ggplot(data_graph) +
  aes(x=p_values, y=reorder(baseline,-p_values)) +
  geom_point(colour='#0000FF', size=1.5) +
  geom_vline(aes(xintercept=data_graph[baseline=='Left party candidate', p_values]),
             linetype='longdash', colour='#666666') +
  ggtitle('Continuity of means predetermined variable') +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=9),
        axis.title.x = element_text(size =12)) +
  xlab('P-value of test for continuity of means around cutoff') +
  ylab(NULL)
ggsave('./output/figures/figureG2b.pdf', width=6.2, height=4.85)



### Figure G2c: Continuity of distribution using asymptotic permutation test
set.seed(234)
permtest <- RDperm(W = baseline,
                   z = "victory_margin",
                   data = hate_crimes_unique)

summary(permtest)
baseline_pv <- unname(permtest$results[,2])
data_graph <- data.frame('baseline'=baseline_labels, 'p_values'=baseline_pv[1:length(baseline_pv)-1])
data_graph <- data.table(data_graph)
data_graph <- data_graph[order(p_values)]

# BH procedure
data_graph[,rank:=as.numeric(rownames(data_graph))]
data_graph[,cv:=(rank/nrow(data_graph))*0.05]

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
ggsave('./output/figures/figureG2c.pdf', width=6.2, height=4.85)
