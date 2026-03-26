
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
  ver12[, number_lec := sum(law_enforcement, na.rm = TRUE), by = .(muni_code)]
  ver12[, number_occ_law := sum(occ_law, na.rm = TRUE), by = .(muni_code)]
  ver12[, number_lists := length(unique(coal_mun_name)), by = .(muni_code)]
  ver12[, number_lists_loc := length(unique(coal_mun_name[law_enforcement == 1])), by = .(muni_code)]
  ver12[, lec_present := ifelse(number_lec > 0 , 1, 0)]
  ver12[, occ_law_present := ifelse(number_occ_law > 0 , 1, 0)]
  ver12[, number_lec_elected := sum(law_enforcement * elected, na.rm = TRUE), by = .(muni_code)]
  ver12[, lec_elected_present := ifelse(number_lec_elected > 0 , 1, 0)]
  ver12[, leftwing:=ifelse(party %in% c('PT','PDT','PSB','PSTU','PCO','PCdoB','PV','PSOL'),1,0)]
  ver12[, leftwing_loc := sum(law_enforcement * leftwing, na.rm = TRUE), by = .(muni_code)]
  ver12[, prop_leftwing := leftwing_loc / number_lec, by = .(muni_code)]
  ver12[, total_council_cands := .N, by = .(muni_code)]

law_enforcement_municipalities = unique(ver12[law_enforcement==1]$muni_code) # note that this selection only works because I am dealing with a single year here., otherwise I would need the (muni_code + year) pair

ver12[, lecm := ifelse(muni_code %in% law_enforcement_municipalities,1,0)]
  
#Comparing municipalitites. Hence, I need to isolate one year+pair for each instance of    
test.data = unique(ver12[,list(lecm, muni_code, dv_homicides_lagged, dv_nonwhite_men_lagged, lag_pop, nonwhite, gini, lag_finbra_period_1, pop, robbery_pc, gdp_pc, lag_assault_period_1, lag_assault_black_men_period_1, number_lec, lec_present, prop_leftwing, lec_elected_present, number_lec_elected, number_lec, number_lists_loc, number_lists, sc_before, marginal_muni, occ_law_present, number_occ_law, total_council_cands)])

test.data[, log_lag_pop := log(lag_pop)]
test.data[, assault_pc := I(lag_assault_period_1/lag_pop)*I(100000/3)]
test.data[, assault_non_white_pc := I(lag_assault_black_men_period_1/I(lag_pop))*I(100000/3)]
test.data[, finbra_pc := I(lag_finbra_period_1/lag_pop)/3]
test.data[, total_munis := .N, by = 'lecm']

tests =lapply(test.data[ , c("nonwhite",'lag_pop', "gini", 'sc_before','gdp_pc', "assault_pc","assault_non_white_pc",'finbra_pc','robbery_pc','number_lists','lec_present','number_lec','number_lists_loc','prop_leftwing','lec_elected_present','number_lec_elected','occ_law_present','number_occ_law','total_council_cands','total_munis')], 
              function(x) mean(x, na.rm = TRUE))

tests_1 =lapply(test.data[test.data$lecm == 1 ,c("nonwhite",'lag_pop', "gini", 'sc_before','gdp_pc', "assault_pc","assault_non_white_pc",'finbra_pc','robbery_pc','number_lists','lec_present','number_lec','number_lists_loc','prop_leftwing','lec_elected_present','number_lec_elected','occ_law_present','number_occ_law','total_council_cands','total_munis')], 
                function(x) mean(x, na.rm = TRUE))

tests_2 =lapply(test.data[test.data$lecm == 0 , c("nonwhite",'lag_pop', "gini", 'sc_before','gdp_pc', "assault_pc","assault_non_white_pc",'finbra_pc','robbery_pc','number_lists','lec_present','number_lec','number_lists_loc','prop_leftwing','lec_elected_present','number_lec_elected','occ_law_present','number_occ_law','total_council_cands','total_munis')], 
                function(x) mean(x, na.rm = TRUE))

tests_3 =lapply(test.data[test.data$lecm == 1 & abs(test.data$marginal_muni) < 0.01, c("nonwhite",'lag_pop', "gini", 'sc_before','gdp_pc', "assault_pc","assault_non_white_pc",'finbra_pc','robbery_pc','number_lists','lec_present','number_lec','number_lists_loc','prop_leftwing','lec_elected_present','number_lec_elected','occ_law_present','number_occ_law','total_council_cands','total_munis')], 
                function(x) mean(x, na.rm = TRUE))

plot.data = data.table(variable = c('Non-white population',
                                    'Population (average)',
                                    'Population (median)',
                                    'Inequality (GINI)',
                                    'Security council',
                                    'GDP per capita',
                                    'Homicide rate',
                                    'Non-white men homicide rate',
                                    'Security spending pc',
                                    'Car robberies pc',
                                    'N. of councilor lists',
                                    'Law-and-order cand.',
                                    'N. of Law-and-order',
                                    'N. of lists w. law-and-order',
                                    '% of left-wing law-and-order',
                                    'Law-and-order elected',
                                    'N. law-and-order elected',
                                    'Law enforcement or military occ.',
                                    'N. Law enforc. or military occ.',
                                    'N. council candidates',
                                    'Total municipalities'),
                       `All` = round(c(tests$nonwhite,
                                       tests$lag_pop,
                                       median(test.data$lag_pop, na.rm = TRUE),
                                       tests$gini,
                                       tests$sc_before,
                                       tests$gdp_pc,
                                       tests$assault_pc,
                                       tests$assault_non_white_pc,
                                       tests$finbra_pc,
                                       tests$robbery_pc,
                                       tests$number_lists,
                                       tests$lec_present,
                                       tests$number_lec,
                                       tests$number_lists_loc,
                                       tests$prop_leftwing,
                                       tests$lec_elected_present,
                                       tests$number_lec_elected,
                                       tests$occ_law_present,
                                       tests$number_occ_law,
                                       tests$total_council_cands,
                                       5568
                       ),2),
                       `Law-and-order` = round(c(tests_1$nonwhite,
                                                 tests_1$lag_pop,
                                                 median(test.data[lecm==1]$lag_pop, na.rm = TRUE),
                                                 tests_1$gini,
                                                 tests_1$sc_before,
                                                 tests_1$gdp_pc,
                                                 tests_1$assault_pc,
                                                 tests_1$assault_non_white_pc,
                                                 tests_1$finbra_pc,
                                                 tests_1$robbery_pc,
                                                 tests_1$number_lists,
                                                 tests_1$lec_present,
                                                 tests_1$number_lec,
                                                 tests_1$number_lists_loc,
                                                 tests_1$prop_leftwing,
                                                 tests_1$lec_elected_present,
                                                 tests_1$number_lec_elected,
                                                 tests_1$occ_law_present,
                                                 tests_1$number_occ_law,
                                                 tests_1$total_council_cands,
                                                 tests_1$total_munis  
                        ),2),
                       `L&O, close margin` = round(c(tests_3$nonwhite,
                                                 tests_3$lag_pop,
                                                 median(test.data[lecm==1 & abs(marginal_muni) < 0.01]$lag_pop, na.rm = TRUE),
                                                 tests_3$gini,
                                                 tests_3$sc_before,
                                                 tests_3$gdp_pc,
                                                 tests_3$assault_pc,
                                                 tests_3$assault_non_white_pc,
                                                 tests_3$finbra_pc,
                                                 tests_3$robbery_pc,
                                                 tests_3$number_lists,
                                                 tests_3$lec_present,
                                                 tests_3$number_lec,
                                                 tests_3$number_lists_loc,
                                                 tests_3$prop_leftwing,
                                                 tests_3$lec_elected_present,
                                                 tests_3$number_lec_elected,
                                                 tests_3$occ_law_present,
                                                 tests_3$number_occ_law,
                                                 tests_3$total_council_cands,
                                                 516  
                       ),2),
                       `No Law-and-order` = round(c(tests_2$nonwhite,
                                                 tests_2$lag_pop,
                                                 median(test.data[lecm==0]$lag_pop, na.rm = TRUE),
                                                 tests_2$gini,
                                                 tests_2$sc_before,
                                                 tests_2$gdp_pc,
                                                 tests_2$assault_pc,
                                                 tests_2$assault_non_white_pc,
                                                 tests_2$finbra_pc,
                                                 tests_2$robbery_pc,
                                                 tests_2$number_lists,
                                                 tests_2$lec_present,
                                                 tests_2$number_lec,
                                                 tests_2$number_lists_loc,
                                                 0,
                                                 tests_2$lec_elected_present,
                                                 tests_2$number_lec_elected,
                                                 tests_2$occ_law_present,
                                                 tests_2$number_occ_law,
                                                 tests_2$total_council_cands,
                                                 tests_2$total_munis
                       ),2))

test <- xtable(plot.data, 
               size=small,
               label = 'comparison table', 
               caption = 'Comparing municipalities that had a law enforcement candidate (LEC) and those that have not, 2012')

print(test,include.rownames=F,size='footnotesize')


