################
# ATTENTION: THIS ONLY RUNS ON OLDER VERSIONS OF R DUE TO STARGAZER. Run IN R 4.1.1
##################

rm(list = ls())
library(data.table)
library(stargazer)
library(multiwayvcov)
library(lmtest)
library(here)

dados_all <- readRDS(here('data','data_census.rds'))

dados_test <- dados_all[p_elected ==0 & police_cand_elected==1]

# Models ----

model_flagrante.1000 <- as.formula("ratio_2012*100 ~ I(no_support * lec_elected) + no_support + poor + rich + prop_nonwhite + prop_local_total_young_men + as.factor(muni_code)+total_votes_local")
model_flagrante.500 <- as.formula("ratio_2012_500*100 ~ I(no_support * lec_elected) + no_support  + poor + rich + prop_nonwhite +prop_local_total_young_men +as.factor(muni_code)+total_votes_local")
model_flagrante.250 <- as.formula("ratio_2012_250*100 ~ I(no_support * lec_elected) + no_support  + poor + rich + prop_nonwhite + prop_local_total_young_men + as.factor(muni_code)+total_votes_local")


model_homicides.1000 <- as.formula("penalty_homicide2012 - penalty_homicide2008 ~ I(no_support * lec_elected) + no_support + poor + rich + prop_nonwhite + as.factor(muni_code)+total_votes_local + prop_local_total_young_men")
model_homicides.500 <- as.formula("penalty_homicide2012_500 - penalty_homicide2008_500 ~ I(no_support * lec_elected) + no_support + poor + rich + prop_nonwhite  +as.factor(muni_code)+total_votes_local + prop_local_total_young_men")
model_homicides.250 <- as.formula("penalty_homicide2012_250 - penalty_homicide2008_250 ~ I(no_support * lec_elected) + no_support + poor + rich + prop_nonwhite + as.factor(muni_code)+total_votes_local + prop_local_total_young_men")

# Estimations
## Flagrante

# 250m 
  cluster_lec.250_f <- lm(model_flagrante.250,data=dados_test)
  vcov_lec_250_f <- cluster.vcov(cluster_lec.250_f, dados_test$muni_code)
  cluster.250_f = coeftest(cluster_lec.250_f, vcov_lec_250_f)

# 500 m
  cluster_lec.500_f <- lm(model_flagrante.500,data=dados_test)
  vcov_lec_500_f <- cluster.vcov(cluster_lec.500_f, dados_test$muni_code)
  cluster.500_f = coeftest(cluster_lec.500_f, vcov_lec_500_f)

# 1000 m
  cluster_lec.1000_f <- lm(model_flagrante.1000,data=dados_test)
  vcov_lec_1000_f <- cluster.vcov(cluster_lec.1000_f, dados_test$muni_code)
  cluster.1000_f = coeftest(cluster_lec.1000_f, vcov_lec_1000_f)

## Homicides

# 250 m
  cluster_lec.250 <- lm(model_homicides.250,data=dados_test)
  vcov_lec_250 <- cluster.vcov(cluster_lec.250, dados_test$muni_code)
  cluster.250 = coeftest(cluster_lec.250, vcov_lec_250)

# 500 m
  cluster_lec.500 <- lm(model_homicides.500,data=dados_test)
  vcov_lec_500 <- cluster.vcov(cluster_lec.500, dados_test$muni_code)
  cluster.500 = coeftest(cluster_lec.500, vcov_lec_500)

# 1000 m
  cluster_lec.1000 <- lm(model_homicides.1000,data=dados_test)
  vcov_lec_1000 <- cluster.vcov(cluster_lec.1000, dados_test$muni_code)
  cluster.1000 = coeftest(cluster_lec.1000, vcov_lec_1000)

# Table 2
table_police <- stargazer(cluster.250_f, cluster.500_f, cluster.1000_f, cluster.250, cluster.500, cluster.1000,
                          stars='default',
                          digits=2,
                          title = 'Support for Law Enforcement Candidates (LEC), Police Activity, and Homicides',
                          #type='text',
                          omit = c("muni_code",'z.prop_local_total_high_school','z.prop_local_total_high_school',"z.prop_local_total_college","total_votes_local",'poor','rich','prop_nonwhite','prop_local_total_young_men','Constant'),
                          add.lines = list(c("Radius",rep(c("0.25 km","0.5 km","1.0 km"),2))
                                           ,c("P.St. controls",rep(c("Y"),8))
                                           ,c("Munic. FE",rep(c("Y"),8))
                                           ,c('N. obs',nobs(cluster_lec.250_f),nobs(cluster_lec.500_f),nobs(cluster_lec.1000_f),
                                              nobs(cluster_lec.250),nobs(cluster_lec.500),nobs(cluster_lec.1000))
                                           #,c("N. Municipalities","-","Y","Y","Y","Y","Y")
                                           #,c("Clustered SEs","-","-","Y","-","-","Y")
                          ),
                          covariate.labels = c('Low support'),
                          dep.var.caption = 'Variation, 2012-2016',
                          column.separate = c(3, 3), column.labels = c("Police Activity", "Homicides"),
                          omit.table.layout = 'dm'
)

#Table B5
table_police_appendix <- stargazer(cluster.250_f, cluster.500_f, cluster.1000_f, cluster.250, cluster.500, cluster.1000,
                                   stars='default',
                                   digits=2,
                                   title = 'Support for Law Enforcement Candidates (LEC), Police Activity, and Homicides',
                                   #type='text',
                                   omit = c("muni_code"),
                                   add.lines = list(c("Radius",rep(c("0.25 km","0.5 km","1.0 km"),2))
                                                    ,c("P.St. controls",rep(c("Y"),8))
                                                    ,c("Munic. FE",rep(c("Y"),8))
                                                    ,c('N. obs',nobs(cluster_lec.250_f),nobs(cluster_lec.500_f),nobs(cluster_lec.1000_f),
                                                       nobs(cluster_lec.250),nobs(cluster_lec.500),nobs(cluster_lec.1000))
                                                    #,c("N. Municipalities","-","Y","Y","Y","Y","Y")
                                                    #,c("Clustered SEs","-","-","Y","-","-","Y")
                                   ),
                                   covariate.labels = c('Low support'),
                                   dep.var.caption = 'Variation, 2012-2016',
                                   column.separate = c(3, 3), column.labels = c("Police Activity", "Homicides"),
                                   omit.table.layout = 'dm'
)
# Security committees ----

dados_sc <- dados_all[p_elected ==0 & sc_before == 1 & police_cand_elected == 1]
dados_nsc <- dados_all[p_elected ==0 & sc_before == 0 & police_cand_elected == 1]

# Homicide

# 250 m
cluster_lec.250 <- lm(model_homicides.250,data=dados_nsc)
vcov_lec_250 <- cluster.vcov(cluster_lec.250, dados_nsc$muni_code)
cluster.250 = coeftest(cluster_lec.250, vcov_lec_250)

# 500 m
cluster_lec.500 <- lm(model_homicides.500,data=dados_nsc)
vcov_lec_500 <- cluster.vcov(cluster_lec.500, dados_nsc$muni_code)
cluster.500 = coeftest(cluster_lec.500, vcov_lec_500)

# 1000 m
cluster_lec.1000 <- lm(model_homicides.1000,data=dados_nsc)
vcov_lec_1000 <- cluster.vcov(cluster_lec.1000, dados_nsc$muni_code)
cluster.1000 = coeftest(cluster_lec.1000, vcov_lec_1000)

## SC ----
# 250 m
cluster_lec.250_sc <- lm(model_homicides.250,data=dados_sc)
vcov_lec_250 <- cluster.vcov(cluster_lec.250_sc, dados_sc$muni_code)
cluster.250_sc = coeftest(cluster_lec.250_sc, vcov_lec_250)

# 500 m
cluster_lec.500_sc <- lm(model_homicides.500,data=dados_sc)
vcov_lec_500 <- cluster.vcov(cluster_lec.500_sc, dados_sc$muni_code)
cluster.500_sc = coeftest(cluster_lec.500_sc, vcov_lec_500)

# 1000 m
cluster_lec.1000_sc <- lm(model_homicides.1000,data=dados_sc)
vcov_lec_1000 <- cluster.vcov(cluster_lec.1000_sc, dados_sc$muni_code)
cluster.1000_sc = coeftest(cluster_lec.1000_sc, vcov_lec_1000)

# Table A10
table_police <- stargazer(cluster.250, cluster.500, cluster.1000, cluster.250_sc, cluster.500_sc, cluster.1000_sc,
                          stars='default',
                          digits = 2,
                          title = 'Support for Law Enforcement Candidates (LEC) and Homicides',
                          type='latex',
                          omit = c("muni_code"),
                          add.lines = list(c("Radius",rep(c("0.25 km","0.5 km","1 km"),2))
                                           ,c("P.St. controls",rep(c("Y"),8))
                                           ,c("Munic. FE",rep(c("Y"),8))
                                           ,c('N. obs',nobs(cluster_lec.250),nobs(cluster_lec.500),nobs(cluster_lec.250),
                                              nobs(cluster_lec.250_sc),nobs(cluster_lec.500_sc),nobs(cluster_lec.250_sc))
                                           #,c("N. Municipalities","-","Y","Y","Y","Y","Y")
                                           #,c("Clustered SEs","-","-","Y","-","-","Y")
                          ),
                          covariate.labels = c("Low support"),
                          dep.var.caption = 'Variation, 2012-2016',
                          column.separate = c(3, 3), column.labels = c("No Security Committee", "Security Committee"),
                          label = 'geo homicides sc',
                          omit.table.layout = 'dm'
)

# Flagrante 


## No SC
# 250 m
cluster_lec.250 <- lm(model_flagrante.250,data=dados_nsc)
vcov_lec_250 <- cluster.vcov(cluster_lec.250, dados_nsc$muni_code)
cluster.250 = coeftest(cluster_lec.250, vcov_lec_250)

# 500 m
cluster_lec.500 <- lm(model_flagrante.500,data=dados_nsc)
vcov_lec_500 <- cluster.vcov(cluster_lec.500, dados_nsc$muni_code)
cluster.500 = coeftest(cluster_lec.500, vcov_lec_500)

# 1000 m
cluster_lec.1000 <- lm(model_flagrante.1000,data=dados_nsc)
vcov_lec_1000 <- cluster.vcov(cluster_lec.1000, dados_nsc$muni_code)
cluster.1000 = coeftest(cluster_lec.1000, vcov_lec_1000)

## SC
# 250 m
cluster_lec.250_sc <- lm(model_flagrante.250,data=dados_sc)
vcov_lec_250 <- cluster.vcov(cluster_lec.250_sc, dados_sc$muni_code)
cluster.250_sc = coeftest(cluster_lec.250_sc, vcov_lec_250)

# 500 m
cluster_lec.500_sc <- lm(model_flagrante.500,data=dados_sc)
vcov_lec_500 <- cluster.vcov(cluster_lec.500_sc, dados_sc$muni_code)
cluster.500_sc = coeftest(cluster_lec.500_sc, vcov_lec_500)

# 1000 m
cluster_lec.1000_sc <- lm(model_flagrante.1000,data=dados_sc)
vcov_lec_1000 <- cluster.vcov(cluster_lec.1000_sc, dados_sc$muni_code)
cluster.1000_sc = coeftest(cluster_lec.1000_sc, vcov_lec_1000)

# Table A11
table_police <- stargazer(cluster.250, cluster.500, cluster.1000, cluster.250_sc, cluster.500_sc, cluster.1000_sc,
                          stars='default',
                          digits=1,
                          title = 'Support for Law Enforcement Candidates (LEC) and Police Activity',
                          type='latex',
                          omit = c("muni_code"),
                          add.lines = list(c("Radius",rep(c("0.25 km","0.5 km","1 km"),2))
                                           ,c("P.St. controls",rep(c("Y"),8))
                                           ,c("Munic. FE",rep(c("Y"),8))
                                           ,c('N. obs',nobs(cluster_lec.250),nobs(cluster_lec.500),nobs(cluster_lec.250),
                                              nobs(cluster_lec.250_sc),nobs(cluster_lec.500_sc),nobs(cluster_lec.1000_sc))),
                          dep.var.caption = 'Variation, 2012-2016',
                          label = 'geo flagrante sc',
                          covariate.labels = c("Low support"),
                          column.separate = c(3, 3), column.labels = c("No Security Committee", "Security Committee"),
                          omit.table.layout = 'dm'
)
