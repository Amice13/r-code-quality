
#Please restart R
options(scipen=999)
rm(list = ls())
library(grid)
library(data.table)
library(sandwich)
library(reshape2)
library(xtable)
library(here)

load(here("data","ver_data.RData"))

ver12 <- ver_b[year==2012]
law_enforcement_municipalities = unique(ver12[law_enforcement==1]$muni_code)

ver12[,lecm:=ifelse(muni_code %in% law_enforcement_municipalities,1,0)]
ver12[,lecm_elected:=ifelse(muni_code %in% law_enforcement_municipalities & elected == 1,1,0)]
ver12[,lec_sc:=ifelse(muni_code %in% law_enforcement_municipalities & sc_before == 1,1,0)]

#Comparing municipalitites. Hence, I need to isolate one year+pair for each instance of    
test.data = unique(ver12[,list(uf, lecm, lecm_elected, lec_sc, muni_code, dv_homicides_lagged, dv_nonwhite_men_lagged, dv_white_men ,lag_pop, nonwhite, gini, dv_finbra_lagged, pop, robbery_pc, gdp_pc)])

test.data[, lag_pop_k := lag_pop/1000]

tests =lapply(test.data[lecm_elected==1,c("nonwhite",'lag_pop_k', "gini",'gdp_pc', "dv_homicides_lagged","dv_nonwhite_men_lagged",'dv_finbra_lagged')], function(x) t.test(x ~ test.data[lecm_elected==1]$lec_sc,alternative='two.sided',conf.level=0.95))

plot.data = data.table(variable = c('Non-White Population',
                                    'Population (in thousands)',
                                    'Inequality (Gini)',
                                    'GDP per capita, in reais',
                                    'Variation in homicides',
                                    'Variation in homicides of non-white men',
                                    'Variation in security spending, in reais pc'),
                        `With Committee` = round(c(tests$nonwhite$estimate[[2]],
                                           tests$lag_pop$estimate[[2]],
                                           tests$gini$estimate[[2]],
                                           tests$gdp_pc$estimate[[2]],
                                           tests$dv_homicide$estimate[[2]],
                                           tests$dv_nonwhite_men$estimate[[2]],
                                           tests$dv_finbra$estimate[[2]],
                                           tests$robbery_pc$estimate[[2]]
                                            ),2),
                        `Without Committee` = round(c(tests$nonwhite$estimate[[1]],
                                          tests$lag_pop$estimate[[1]],
                                          tests$gini$estimate[[1]],
                                          tests$gdp_pc$estimate[[1]],
                                          tests$dv_homicide$estimate[[1]],
                                          tests$dv_nonwhite_men$estimate[[1]],
                                          tests$dv_finbra$estimate[[1]],
                                          tests$robbery_pc$estimate[[1]]
                                            ),2),
                        p.value = round(c(tests$nonwhite$p.value[[1]],
                                          tests$lag_pop$p.value[[1]],
                                          tests$gini$p.value[[1]],
                                          tests$gdp_pc$p.value[[1]],
                                          tests$dv_homicide$p.value[[1]],
                                          tests$dv_nonwhite_men$p.value[[1]],
                                          tests$dv_finbra$p.value[[1]],
                                          tests$robbery_pc$p.value[[1]]
                                          ),5
                                            ))

# Table A3
test <- xtable(plot.data, size=small, label = 'comparison table', caption = 'Comparing municipalities that had a security committee from those that did not, Brazil - 2012')
print(test,include.rownames=F,size='footnotesize')


#Not in paper

# São Paulo ----

#Comparing municipalitites. Hence, I need to isolate one year+pair for each instance of    
test.data = unique(ver12[uf == 'SP',list(uf, lecm, lecm_elected, lec_sc, muni_code, dv_homicides_lagged, dv_nonwhite_men_lagged, dv_white_men ,lag_pop, nonwhite, gini, dv_finbra_lagged, pop, robbery_pc, gdp_pc)])

test.data[, lag_pop_k := lag_pop/1000]

tests =lapply(test.data[lecm_elected==1,c("nonwhite",'lag_pop_k', "gini",'gdp_pc', "dv_homicides_lagged","dv_nonwhite_men_lagged",'dv_finbra_lagged')], function(x) t.test(x ~ test.data[lecm_elected==1]$lec_sc,alternative='two.sided',conf.level=0.95))

plot.data = data.table(variable = c('Non-White Population',
                                    'Population (in thousands)',
                                    'Inequality (Gini)',
                                    'GDP per capita, in reais',
                                    'Variation in homicides',
                                    'Variation in homicides of non-white men',
                                    'Variation in security spending, in reais pc'),
                       `With Committee` = round(c(tests$nonwhite$estimate[[2]],
                                                  tests$lag_pop$estimate[[2]],
                                                  tests$gini$estimate[[2]],
                                                  tests$gdp_pc$estimate[[2]],
                                                  tests$dv_homicide$estimate[[2]],
                                                  tests$dv_nonwhite_men$estimate[[2]],
                                                  tests$dv_finbra$estimate[[2]],
                                                  tests$robbery_pc$estimate[[2]]
                       ),2),
                       `Without Committee` = round(c(tests$nonwhite$estimate[[1]],
                                                     tests$lag_pop$estimate[[1]],
                                                     tests$gini$estimate[[1]],
                                                     tests$gdp_pc$estimate[[1]],
                                                     tests$dv_homicide$estimate[[1]],
                                                     tests$dv_nonwhite_men$estimate[[1]],
                                                     tests$dv_finbra$estimate[[1]],
                                                     tests$robbery_pc$estimate[[1]]
                       ),2),
                       p.value = round(c(tests$nonwhite$p.value[[1]],
                                         tests$lag_pop$p.value[[1]],
                                         tests$gini$p.value[[1]],
                                         tests$gdp_pc$p.value[[1]],
                                         tests$dv_homicide$p.value[[1]],
                                         tests$dv_nonwhite_men$p.value[[1]],
                                         tests$dv_finbra$p.value[[1]],
                                         tests$robbery_pc$p.value[[1]]
                       ),5
                       ))

test <- xtable(plot.data, size=small, label = 'comparison table', caption = 'Comparing municipalities that had a security committee from those that did not, 2012')
print(test,include.rownames=F,size='footnotesize')
