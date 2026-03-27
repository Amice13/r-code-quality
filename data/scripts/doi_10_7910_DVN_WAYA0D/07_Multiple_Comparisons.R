## Replication code for 'Locked Out of College: ...'
## Jacob R. Brown and Hanno Hilbig (hhilbig@g.harvard.edu)
## Nov 18, 2020
##
## This file: Tables B19, B20

## Set WD

rm(list = ls())
setwd("")

## Packages
## This function auto-installs packages if missing, and then loads them

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

pkgList <- c("tidyverse", "gridExtra", "knitr", "kableExtra")
check.packages(pkgList)

#### Read data ####
## Here we read the saved results from all hypothesis tests conducted in the paper 

res_all <- read_csv('Data_SavedResults.csv')

## Adjust for MC

res_all <- res_all %>% 
  group_by(outcome) %>% 
  mutate(pval_adj_bh = p.adjust(p.value, method = 'BH')) %>% 
  ungroup() %>% 
  arrange(p.value) %>% 
  mutate(controls = ifelse(controls == 'Controls', 
                           '1Controls Included',
                           '2No Controls'))

#### Prep for table output ####

res_all_output <- res_all %>% 
  separate(term, into = c('t1', 't2'), sep = ':') %>% 
  mutate_at(vars(t1, t2),
            list(~dplyr::recode(.,
                       `treat_felon` = 'Criminal Record',
                       `treat_black` = 'Black',
                       `treat_advocateWhite Advocate` = 'White Advocate',
                       `treat_advocateBlack Advocate` = 'Black Advocate',
                       `covar_public_privatepublic` = 'Public Institution'))) %>% 
  unite(t1, t2, sep = ' * ', col = 'term') %>% 
  mutate(outcome = dplyr::recode(outcome,
                          `response` = '1Response',
                          `thorough` = '2Thoroughness',
                          `friend` = '3Friendliness')) %>% 
  mutate(subset = dplyr::recode(subset, 
                         `none` = '1All Colleges',
                         `2y` = '4Two-year Colleges',
                         `4y` = '5Four-year Colleges',
                         `private` = '2Private Institutions',
                         `public` = '3Public Institutions',
                         `big` =  '6> 5k Students',
                         `small` =  '7< 5k Students')) %>% 
  dplyr::select(term, everything()) %>% 
  mutate(term = str_remove(term, ' \\* NA')) %>% 
  arrange(outcome, subset, term, controls)

## More prep for table output ##

res_main <- res_all_output %>% 
  filter(outcome == '1Response') %>% 
  filter(controls == '1Controls Included') %>% 
  filter(!str_detect(term, 'Advocate')) %>% 
  filter(!term == 'Criminal Record * Black') %>% 
  filter(str_detect(subset, paste0(c('All Colleges', 'Public Institutions', 'Private Institutions'),
                                   collapse = '|'))) %>% 
  mutate(source = c('Figure 1', 'Table B6', 'Figure 1', 'Table B6', 
                        rep('Figure 2', 4))) %>% 
  dplyr::select(term, subset, outcome, controls, source, 
                estimate, std.error, p.value, pval_adj_bh) %>% 
  mutate_at(vars(outcome, subset, controls),
            list(~substring(., 2)))

#### Table B19 : Abridged version of results, adjusted for MC ####

kable(res_main, 
      "latex", longtable = F, 
      booktabs = T, col.names = c('Term',
                                  'Sample', 
                                  'Outcome',
                                  'Controls',
                                  'Shown in', 
                                  'Estimate',
                                  'SE',
                                  'P-val',
                                  'Adjusted P-val'),
      linesep = "",
      label = 'tab:main_appendix',
      escape = F, digits = 3) %>%
  kable_styling(latex_options = c("repeat_header"),
                font_size = 11) %>% 
  # collapse_rows() %>% 
  row_spec(0, bold = T) %>% 
  kable_styling(latex_options = "HOLD_position")

#### Table B20 : Full version of results, adjusted for MC ####

res_all <- res_all_output %>% 
  dplyr::select(term, subset, outcome, controls, 
                estimate, std.error, p.value, pval_adj_bh) %>% 
  arrange(outcome, subset, term, controls) %>% 
    mutate_at(vars(outcome, subset, controls),
              list(~substring(., 2)))

kable(res_all, 
      "latex", longtable = T, 
      booktabs = T, col.names = c('Term',
                                  'Sample', 
                                  'Outcome',
                                  'Controls',
                                  'Estimate',
                                  'SE',
                                  'P-val',
                                  'Adjusted P-val'),
      linesep = "",
      escape = F, digits = 3) %>%
  kable_styling(latex_options = c("repeat_header"),
                font_size = 11) %>% 
  # collapse_rows() %>% 
  row_spec(0, bold = T) %>% 
  kable_styling(latex_options = "HOLD_position")

 