##########################################################################
# EFFECTS OF CAMPAIGN LIMITS ON ENTRY OF REPUBLICANOS' CANDIDATES
###########################################################################

rm(list=ls())
library(data.table)
library(rdrobust)
library(here)
library(tidyr)
library(kableExtra)
library(modelsummary)

electoral <- readRDS(here("data","mayors_municipal_level.rds"))

# TABLE
#packages for the tables

# Load functions for modelsummary 

tidy.rdrobust <- function(object, ...) {
  ret <- data.frame(
    term = 'Robust Coef.',
    estimate = object$coef[3, 1],
    std.error = object$se[3, 1],
    p.value = object$pv[3, 1]
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(object, ...) {
  ret <- data.frame(
    #Kernel = object$kernel,
    Bandwidth = paste('R$', round(object$bws[2,2],0)),
    #Bw.Selection = object$bwselect,
    Effective_N.obs = as.character(object$N_h[[1]] + object$N_h[[2]])
  )
  ret
}

# TABLES CANDIDACIES ----

prb_candidate_table <- rdrobust(electoral$prb_candidate_2016, electoral$inverted_margin,
                                all=TRUE)

prb_candidate_table_0 <- rdrobust(electoral$prb_candidate_2016, 
                                     electoral$inverted_margin, all=TRUE, p=0)

prb_candidate_table_cerrd <- rdrobust(electoral$prb_candidate_2016, electoral$inverted_margin, all=TRUE, bwselect = 'cercomb1')

prb_candidate_table_2nd <- rdrobust(electoral$prb_candidate_2016, electoral$inverted_margin, all=TRUE, p= 2)

table_prb <- list('Fielded, 2016' = prb_candidate_table, 
                  'Fielded, 2016' = prb_candidate_table_cerrd,
                  'Fielded, 2016' = prb_candidate_table_0,
                  'Fielded, 2016' = prb_candidate_table_2nd
                  )

row1 <- c('Bandwidth selector','Mean square error',' Coverage error','Mean square error','Mean square error')
row2 <- c('Order polynomial','1','1','0','2')

 add_this <- data.frame(t(data.frame(row1,row2)))

 
modelsummary(table_prb, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             add_rows = add_this,
             title = 'Effects of Campaign Spending on Republicanos\' Mayoral Candidacies')
# TABLE Candidacies, pre-treatment

prb_candidate_table <- rdrobust(electoral$prb_candidate_2012, electoral$inverted_margin,
                                all=TRUE)

prb_candidate_table_0 <- rdrobust(electoral$prb_candidate_2012, 
                                  electoral$inverted_margin, all=TRUE, p=0)

prb_candidate_table_cerrd <- rdrobust(electoral$prb_candidate_2012, electoral$inverted_margin, all=TRUE, bwselect = 'cercomb1')

prb_candidate_table_2nd <- rdrobust(electoral$prb_candidate_2012, electoral$inverted_margin, all=TRUE, p= 2)

table_prb <- list('Fielded, 2012' = prb_candidate_table, 
                  'Fielded, 2012' = prb_candidate_table_cerrd,
                  'Fielded, 2012' = prb_candidate_table_0,
                  'Fielded, 2012' = prb_candidate_table_2nd
)

row1 <- c('Bandwidth selector','Mean square error',' Coverage error','Mean square error','Mean square error')
row2 <- c('Order polynomial','1','1','0','2')

add_this <- data.frame(t(data.frame(row1,row2)))

modelsummary(table_prb, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             add_rows = add_this,
             title = 'Effects of Campaign Spending on Republicanos\' Mayoral Candidacies (pre-treament') 

# TABLES VICTORIES ----

prb_elected_table <- rdrobust(electoral$prb_elected_2016, electoral$inverted_margin,
                                all=TRUE)

prb_elected_table_0 <- rdrobust(electoral$prb_elected_2016, 
                                  electoral$inverted_margin, all=TRUE, p=0)

prb_elected_table_cerrd <- rdrobust(electoral$prb_elected_2016, electoral$inverted_margin, all=TRUE, bwselect = 'cercomb1')

prb_elected_table_2nd <- rdrobust(electoral$prb_elected_2016, electoral$inverted_margin, all=TRUE, p= 2)

table_prb <- list('Won, 2016' = prb_elected_table, 
                  'Won, 2016' = prb_elected_table_cerrd,
                  'Won, 2016' = prb_elected_table_0,
                  'Won, 2016' = prb_elected_table_2nd
)

row1 <- c('Bandwidth selector','Mean square error',' Coverage error','Mean square error','Mean square error')
row2 <- c('Order polynomial','1','1','0','2')

add_this <- data.frame(t(data.frame(row1,row2)))

modelsummary(table_prb, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             add_rows = add_this,
             title = 'Effects of Campaign Spending on Republicanos\' Mayoral Victories') 
# TABLE victories, pre-treatment

prb_elected_table <- rdrobust(electoral$prb_elected_2012, electoral$inverted_margin,
                                all=TRUE)

prb_elected_table_0 <- rdrobust(electoral$prb_elected_2012, 
                                  electoral$inverted_margin, all=TRUE, p=0)

prb_elected_table_cerrd <- rdrobust(electoral$prb_elected_2012, electoral$inverted_margin, all=TRUE, bwselect = 'cercomb1')

prb_elected_table_2nd <- rdrobust(electoral$prb_elected_2012, electoral$inverted_margin, all=TRUE, p= 2)

table_prb <- list('Won, 2012' = prb_elected_table, 
                  'Won, 2012' = prb_elected_table_cerrd,
                  'Won, 2012' = prb_elected_table_0,
                  'Won, 2012' = prb_elected_table_2nd
)

row1 <- c('Bandwidth selector','Mean square error',' Coverage error','Mean square error','Mean square error')
row2 <- c('Order polynomial','1','1','0','2')

add_this <- data.frame(t(data.frame(row1,row2)))

modelsummary(table_prb, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             add_rows = add_this,
             title = 'Effects of Campaign Spending on Republicanos\' Mayoral Victories (pre-treatment')

# Table Other Parties ----

# Candidates

mdb_candidate <- rdrobust(electoral$mdb_candidate_2016, electoral$inverted_margin,
                              all=TRUE)

pp_candidate <- rdrobust(electoral$pp_candidate_2016, electoral$inverted_margin,
                          all=TRUE)

psc_candidate <- rdrobust(electoral$psc_candidate_2016, electoral$inverted_margin,
                          all=TRUE)

psd_candidate <- rdrobust(electoral$psd_candidate_2016, electoral$inverted_margin,
                          all=TRUE)

psdb_candidate <- rdrobust(electoral$psdb_candidate_2016, electoral$inverted_margin,
                          all=TRUE)

pt_candidate <- rdrobust(electoral$pt_candidate_2016, electoral$inverted_margin,
                           all=TRUE)

small_candidate <- rdrobust(electoral$small_cand_2016, electoral$inverted_margin,
                           all=TRUE)

table_others <- list('MDB' = mdb_candidate, 
                  'PP' = pp_candidate, 
                  'PSC' = psc_candidate, 
                  'PSD' = psd_candidate, 
                  'PSDB' = psdb_candidate, 
                  'PT' = pt_candidate, 
                  'Small parties' = small_candidate)

#row1 <- c('Bandwidth selector','Mean square error',' Coverage error','Mean square error','Mean square error')
#row2 <- c('Order polynomial','1','1','0','2')

#add_this <- data.frame(t(data.frame(row1,row2)))

modelsummary(table_others, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
#             add_rows = add_this,
             title = 'Effects of (stricter) campaign spending limits on fielding candidates for selected parties') 

# victories


mdb_elected <- rdrobust(electoral$mdb_elected_2016, electoral$inverted_margin,
                          all=TRUE)

pp_elected <- rdrobust(electoral$pp_elected_2016, electoral$inverted_margin,
                         all=TRUE)

psc_elected <- rdrobust(electoral$psc_elected_2016, electoral$inverted_margin,
                          all=TRUE)

psd_elected <- rdrobust(electoral$psd_elected_2016, electoral$inverted_margin,
                          all=TRUE)

psdb_elected <- rdrobust(electoral$psdb_elected_2016, electoral$inverted_margin,
                           all=TRUE)

pt_elected <- rdrobust(electoral$pt_elected_2016, electoral$inverted_margin,
                         all=TRUE)

small_elected <- rdrobust(electoral$small_cand_2016, electoral$inverted_margin,
                            all=TRUE)

table_others_elected <- list('MDB' = mdb_elected, 
                     'PP' = pp_elected, 
                     'PSC' = psc_elected, 
                     'PSD' = psd_elected, 
                     'PSDB' = psdb_elected, 
                     'PT' = pt_elected, 
                     'Small parties' = small_elected)

#row1 <- c('Bandwidth selector','Mean square error',' Coverage error','Mean square error','Mean square error')
#row2 <- c('Order polynomial','1','1','0','2')

#add_this <- data.frame(t(data.frame(row1,row2)))

modelsummary(table_others_elected, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             #             add_rows = add_this,
             title = 'Effects of (stricter) campaign spending limits on winning for selected parties')

# Party switching ----

prb_switching <- rdrobust(electoral$change_prb_cand_16, electoral$inverted_margin,
                        all=TRUE)

psc_switching <- rdrobust(electoral$change_psc_cand_16, electoral$inverted_margin,
                       all=TRUE)

table_switching <- list('Republicanos' = prb_switching, 
                             'PSC' = psc_switching)


modelsummary(table_switching, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             #             add_rows = add_this,
             title = 'Effects of (stricter) campaign spending limits on party switching') 

# Centralization and AG ----

# candidate
  psc_above <- with(electoral[HHI_ag > median(electoral$HHI_ag, na.rm = TRUE)], rdrobust(psc_candidate_2016, inverted_margin, all=TRUE))
  
  psc_below <- with(electoral[HHI_ag < median(electoral$HHI_ag, na.rm = TRUE)], rdrobust(psc_candidate_2016, inverted_margin, all=TRUE))
  
  psc_monopolistic <- with(electoral[assembleia_2014  == 1], rdrobust(psc_candidate_2016, inverted_margin, all=TRUE))
  
  psc_competition <- with(electoral[assembleia_2014 > 1], rdrobust(psc_candidate_2016, inverted_margin, all=TRUE))


table_ag_centralization <- list('Decentralized AG' = psc_above, 
                             'Centralized AG' = psc_below,
                             'Single AG' = psc_monopolistic,
                             'Two or more AG' = psc_competition 
                             )

modelsummary(table_ag_centralization, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             #             add_rows = add_this,
             title = 'Campaign spending effects on the PSC candidates, conditional on local centralization and competition among Assembly of God Churches.') 

# Elected

psc_above <- with(electoral[HHI_ag > median(electoral$HHI_ag, na.rm = TRUE)], rdrobust(psc_elected_2016, inverted_margin, all=TRUE))

psc_below <- with(electoral[HHI_ag < median(electoral$HHI_ag, na.rm = TRUE)], rdrobust(psc_elected_2016, inverted_margin, all=TRUE))

psc_monopolistic <- with(electoral[assembleia_2014  == 1], rdrobust(psc_elected_2016, inverted_margin, all=TRUE))

psc_competition <- with(electoral[assembleia_2014 > 1], rdrobust(psc_elected_2016, inverted_margin, all=TRUE))


table_ag_centralization_elected <- list('Decentralized AG' = psc_above, 
                                'Centralized AG' = psc_below,
                                'Single AG' = psc_monopolistic,
                                'Two or more AG' = psc_competition 
)

modelsummary(table_ag_centralization_elected, 
             statistic = c("[{std.error}]",'p.value'),
             #output = 'latex',
             #             add_rows = add_this,
             title = 'Campaign spending effects on the PSC electeds, conditional on local centralization and competition among Assembly of God Churches.') 


# Church presence ----

# UCKG

electoral[, iurd_present_2020 := ifelse(iurd_2020 > 0 ,1 ,0)]
electoral[, iurd_present_2019 := ifelse(iurd_2019 > 0 ,1 ,0)]
electoral[, iurd_present_2018 := ifelse(iurd_2018 > 0 ,1 ,0)]
electoral[, iurd_present_2017 := ifelse(iurd_2017 > 0 ,1 ,0)]
electoral[, iurd_present_2016 := ifelse(iurd_2016 > 0 ,1 ,0)]
electoral[, iurd_present_2015 := ifelse(iurd_2015 > 0 ,1 ,0)]
electoral[, iurd_present_2014 := ifelse(iurd_2014 > 0 ,1 ,0)]
electoral[, iurd_present_2013 := ifelse(iurd_2013 > 0 ,1 ,0)]
electoral[, iurd_present_2012 := ifelse(iurd_2012 > 0 ,1 ,0)]


iurd_2013 <- with(electoral, rdrobust(y = iurd_2013 - iurd_2012, inverted_margin, all=TRUE))
iurd_2014 <- with(electoral, rdrobust(y = iurd_2014 - iurd_2012, inverted_margin, all=TRUE))
iurd_2015 <- with(electoral, rdrobust(y = iurd_2015 - iurd_2012, inverted_margin, all=TRUE))
iurd_2016 <- with(electoral, rdrobust(y = iurd_2016 - iurd_2012, inverted_margin, all=TRUE))
iurd_2017 <- with(electoral, rdrobust(y = iurd_2017 - iurd_2012, inverted_margin, all=TRUE))
iurd_2018 <- with(electoral, rdrobust(y = iurd_2018 - iurd_2012, inverted_margin, all=TRUE))
iurd_2019 <- with(electoral, rdrobust(y = iurd_2019 - iurd_2012, inverted_margin, all=TRUE))
iurd_2020 <- with(electoral, rdrobust(y = iurd_2020 - iurd_2012, inverted_margin, all=TRUE))

table_iurd <- list('2013' = iurd_2013,
                                        '2014' = iurd_2014,
                                        '2015' = iurd_2015,
                                        '2016' = iurd_2016,
                                        '2017' = iurd_2017,
                                        '2018' = iurd_2018,
                                        '2019' = iurd_2019,
                                        '2020' = iurd_2020)

modelsummary(table_iurd, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             #             add_rows = add_this,
             title = 'The Effect of Campaign Spending Limits on the Presence of UCKG Branches.') 
# AG

electoral[, assembleia_present_2020 := ifelse(assembleia_2020 > 0 ,1 ,0)]
electoral[, assembleia_present_2019 := ifelse(assembleia_2019 > 0 ,1 ,0)]
electoral[, assembleia_present_2018 := ifelse(assembleia_2018 > 0 ,1 ,0)]
electoral[, assembleia_present_2017 := ifelse(assembleia_2017 > 0 ,1 ,0)]
electoral[, assembleia_present_2016 := ifelse(assembleia_2016 > 0 ,1 ,0)]
electoral[, assembleia_present_2015 := ifelse(assembleia_2015 > 0 ,1 ,0)]
electoral[, assembleia_present_2014 := ifelse(assembleia_2014 > 0 ,1 ,0)]
electoral[, assembleia_present_2013 := ifelse(assembleia_2013 > 0 ,1 ,0)]
electoral[, assembleia_present_2012 := ifelse(assembleia_2012 > 0 ,1 ,0)]


assembleia_2013 <- with(electoral, rdrobust(y = assembleia_2013 - assembleia_2012, inverted_margin, all=TRUE))
assembleia_2014 <- with(electoral, rdrobust(y = assembleia_2014 - assembleia_2012, inverted_margin, all=TRUE))
assembleia_2015 <- with(electoral, rdrobust(y = assembleia_2015 - assembleia_2012, inverted_margin, all=TRUE))
assembleia_2016 <- with(electoral, rdrobust(y = assembleia_2016 - assembleia_2012, inverted_margin, all=TRUE))
assembleia_2017 <- with(electoral, rdrobust(y = assembleia_2017 - assembleia_2012, inverted_margin, all=TRUE))
assembleia_2018 <- with(electoral, rdrobust(y = assembleia_2018 - assembleia_2012, inverted_margin, all=TRUE))
assembleia_2019 <- with(electoral, rdrobust(y = assembleia_2019 - assembleia_2012, inverted_margin, all=TRUE))
assembleia_2020 <- with(electoral, rdrobust(y = assembleia_2020 - assembleia_2012, inverted_margin, all=TRUE))

table_assembleia <- list('2013' = assembleia_2013,
                                        '2014' = assembleia_2014,
                                        '2015' = assembleia_2015,
                                        '2016' = assembleia_2016,
                                        '2017' = assembleia_2017,
                                        '2018' = assembleia_2018,
                                        '2019' = assembleia_2019,
                                        '2020' = assembleia_2020)

modelsummary(table_assembleia, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             #             add_rows = add_this,
             title = 'The Effect of Campaign Spending Limits on the Presence of AG Branches.') 