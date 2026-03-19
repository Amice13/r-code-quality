rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(tidyr)
library(rdrobust)
library(RATest)
library(ggplot2)
library(forcats)
library(haven)

## load data
bes_rdd_before <- readRDS(file='./data/attitudes/attitudes_before_election.RDS')
bes_rdd_before <- bes_rdd_before[grepl('^(E|W)', ons_id) & ethnicity<3]

load(file='./data/attitudes/attitudes.rds')
bes_rdd <- bes_rdd[grepl('^(E|W)', ons_id) & white==1]

### Figure I3a
## compute RD estimates before the election
main_before <- rdrobust(bes_rdd_before[,immigrants_economy],
                 bes_rdd_before[,victory_margin],
                 kernel = "triangular", p = 1,
                 bwselect = "mserd")

estimates_before <- data.frame(coef = main_before$coef[1,1], cil = main_before$ci[3,1], cir = main_before$ci[3,2], sample = 'Pre')



## compute RD estimates after the election
main_after <- rdrobust(bes_rdd[,immigrants_economy],
                       bes_rdd[,victory_margin],
                       kernel = "triangular", p = 1,
                       bwselect = "mserd",
                       cluster = bes_rdd[, cluster])

estimates_after <- data.frame(coef = main_after$coef[1,1], cil = main_after$ci[3,1], cir = main_after$ci[3,2], sample = 'Post')

estimates <- rbind(estimates_before, estimates_after)

ggplot(estimates,
       aes(y=coef, x=fct_rev(sample))) + geom_point(color = "#2f3132") +
  geom_errorbar(aes(ymin=cil, ymax=cir), width=.1, color = "#2f3132") +
  xlab(NULL) +
  ylab(paste("Minority victory effects on inclusionary attitudes",
             "(immigration good for economy)",
             sep='\n')) +
  geom_hline(yintercept=0, linetype='longdash', size=0.2) +
  theme(legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size =14), axis.title.y = element_text(size =14),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12),
        plot.title = element_text(size=18, lineheight=3))
ggsave(file = './output/figures/figureI3a.pdf', width=6, height=4.85)



#### Figure I3b. Continuity of means using local linear regression
# continuity of predetermined variables (respondents and constituencies)
baseline <- c('young',
              'single',
              'employed',
              'own_house',
              'low_income',
              'male',
              'vote_far_right')

baseline_pv <- c()
normalized_effect <- c()
bandwidth <- c()

for (i in baseline) {
  
  rdout <- rdrobust(bes_rdd[[i]],
                    bes_rdd$victory_margin,
                    kernel = "triangular", p = 1, bwselect = "mserd",
                    cluster = bes_rdd$cluster)
  
  baseline_pv <- c(baseline_pv, rdout$pv[3])
  sd <- bes_rdd[abs(victory_margin) <= rdout$bws[1,1],
                sd(get(i),na.rm=TRUE)]
  
}

baseline_labels <- c('Young',
                     'Single',
                     'Employed',
                     'Own house',
                     'Low income',
                     'Male',
                     'Vote far right')

data_graph <- data.frame('baseline'=baseline_labels, 'p_values'=baseline_pv)
data_graph <- data.table(data_graph)
data_graph <- data_graph[order(p_values)]

baseline_const <- c('incumbent',
                    'female',
                    'party_left',
                    'ethnicity_minority',
                    'religion_minority',
                    'migrant_share',
                    'density',
                    'young_const',
                    'single_share',
                    'deprivation1',
                    'deprivation2',
                    'deprivation3',
                    'deprivation4',
                    'grade_ab',
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
bes_rdd_unique <- unique(bes_rdd[, c(baseline_const, 'election', 'ons_id','victory_margin'), with=F])

baseline_pv <- c()

for (i in baseline_const) {
  
  rdout <- rdrobust(bes_rdd_unique[[i]],
                    bes_rdd_unique$victory_margin,
                    kernel = "triangular", p = 1, bwselect = "mserd")
  
  baseline_pv <- c(baseline_pv, rdout$pv[3])
  sd <- bes_rdd_unique[abs(victory_margin) <= rdout$bws[1,1],
                       sd(get(i),na.rm=TRUE)]
  
}

baseline_labels_const <- c('Candidate: incumbent',
                           'Candidate: female',
                           'Candidate: party left',
                           '% Ethnic minority',
                           '% Non-dominant religion',
                           '% Immigrant',
                           'Population density',
                           '% Young',
                           '% Single' ,
                           '% Deprivation level 1',
                           '% Deprivation level 2',
                           '% Deprivation level 3',
                           '% Deprivation level 4',
                           '% Social grade ab',
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

data_graph_const <- data.frame('baseline'=baseline_labels_const, 'p_values'=baseline_pv)
data_graph_const <- data.table(data_graph_const)
data_graph <- rbind(data_graph,data_graph_const)
data_graph <- data_graph[order(p_values)]

# BH control FDR
data_graph[,rank:=as.numeric(rownames(data_graph))]
data_graph[,cv:=(rank/nrow(data_graph))*0.05]

# p-value from BH: 1.257521e-03
# 2/36 significant with BH correction, 3/36 without correction
ggplot(data_graph) +
  aes(x=p_values, y=reorder(baseline,-p_values)) +
  geom_point(colour='#0000FF', size=1.5) +
  geom_vline(aes(xintercept=data_graph[baseline=='Male', p_values]),
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
ggsave('./output/figures/figureI3b.pdf', width=6.2, height=4.85)




##### Figure I3c. Continuity of distribution using asymptotic permutation test for observations near the cutoff
set.seed(42)
# survey characteristics
## can't compute stats for more than half the individual's variables, need to aggregate survey responses by constituency
# results are not sensitive to form of aggregation stat (sum, mean)
bes_rdd_sum <- bes_rdd[, lapply(.SD, function(x) sum(x,na.rm = TRUE)), .SDcols = baseline,
                       by=.(ons_id, election, victory_margin)]

permtest <- RDperm(W = baseline,
                   z = "victory_margin",
                   data = bes_rdd_sum)

baseline_pv <- unname(permtest$results[1:(nrow(permtest$results)-1),2])
data_graph <- data.frame('baseline'=baseline_labels, 'p_values'=baseline_pv)
data_graph <- data.table(data_graph)

# constituency characteristics
## can't compute stats for % young, skipping that variable
permtest <- RDperm(W = baseline_const[c(1:7,9:length(baseline_const))],
                   z = "victory_margin",
                   data = bes_rdd_unique)

baseline_pv <- unname(permtest$results[1:(nrow(permtest$results)-1),2])
data_graph_const <- data.frame('baseline'=baseline_labels_const[c(1:7,9:length(baseline_const))], 'p_values'=baseline_pv)
data_graph_const <- data.table(data_graph_const)

data_graph <- rbind(data_graph,data_graph_const)
data_graph <- data_graph[order(p_values)]
# BH control FDR
data_graph[,rank:=as.numeric(rownames(data_graph))]
data_graph[,cv:=(rank/nrow(data_graph))*0.05]

ggplot(data_graph) +
  aes(x=p_values, y=reorder(baseline,-p_values)) +
  geom_point(colour='#0000FF', size=2) +
  geom_vline(aes(xintercept=data_graph[baseline=='% Single', p_values]),
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
ggsave('./output/figures/figureI3c.pdf', width=6.2, height=4.85)



