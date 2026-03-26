## Replication code for 'Locked Out of College: ...'
## Jacob R. Brown and Hanno Hilbig (hhilbig@g.harvard.edu)
## Nov 18, 2020
##
## This file: Figure B3, Table B12

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

pkgList <- c("tidyverse", "gridExtra", "lfe")
check.packages(pkgList)


#### Read data ####

df <- read.csv('Data_Main.csv',
               stringsAsFactors = F)

## Relevel advocate treatment, base category should be no advocate

df <- df %>% 
  mutate(treat_advocate = factor(treat_advocate,
                                 levels = c("No Advocate",
                                            "White Advocate",
                                            "Black Advocate")))

## Subset to colleges with disclosure data

df <- df %>% 
  filter(!is.na(crimq))

#### P(Disclosure requirement|Private, Public) ####

tab0 <- df %>% 
  group_by(covar_public_private, crimq) %>% 
  summarise(n = n()) %>% 
  pivot_wider(values_from = n, names_from = crimq)
tab0

#### Figure B3 : Disclosure requirements by school type ####

plot_df2 <- tab0 %>% pivot_longer(cols = 2:3) %>% 
  rename(n = value) %>% ungroup() %>% 
  mutate(covar_public_private = str_to_title(covar_public_private)) %>% 
  group_by(covar_public_private) %>% 
  mutate(n_g = sum(n)) %>% 
  mutate(n = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(covar_public_private = paste0(covar_public_private, ' (N=', n_g, ')'))
  
## Plot 
  
p1 <- ggplot(plot_df2, aes(factor(name), n))  +
  geom_bar(stat = 'identity', 
           color = 'black',
           width=0.4,
           size = 0.3,
           fill = 'grey90') + 
  theme_bw() +
  xlab('') + ylab('') + 
  facet_wrap(~covar_public_private, scales = 'free_x') +
  ylab('Share of Institutions') +
  theme(legend.position = 'bottom')
p1

#### Interactions between disclosure reqs and criminal records ####

m0 <- felm(outcome_response ~ treat_felon  +
             crimq +
             crimq*treat_felon, 
           data = df)
m0 %>% broom::tidy() %>% filter(str_detect(term, 'treat_felon'))

m1 <- felm(outcome_response ~ treat_felon  + treat_black + treat_advocate +
             crimq +
             crimq*treat_felon, 
           data = df)
m1 %>% broom::tidy() %>% filter(str_detect(term, 'treat_felon'))

m2 <- felm(outcome_response ~ treat_felon + treat_black + treat_advocate +
             covar_public_private + 
             factor(covar_inst_size_raw) + 
             factor(state) +
             crimq +
             crimq*treat_felon, 
           data = df)
m2 %>% broom::tidy() %>% filter(str_detect(term, 'treat_felon'))

#### Table B12 : Interacting Criminal Record and Disclosure Requirements ####

stargazer::stargazer(list(m0, m1, m2), 
                     keep = c('treat_felon', 
                              'crimq',
                              'crimq*treat_felon',
                              'Constant'), 
                     covariate.labels = c('Criminal Record', 
                                          'Diclosure Required',
                                          'Criminal Record * Diclosure Required',
                                          'Intercept'),
                     add.lines = list(c('Covariates', c("No", "No", "Yes")),
                                      c('State FE', c("No", "No", "Yes")),
                                      c('Remaining Treatments', c("No", "Yes", "Yes"))),
                     column.labels = c('Bla',
                                       'Bla',
                                       'Bla'),
                     style = 'ajps', 
                     keep.stat = c('n', 'rsq'),
                     dep.var.labels = 'Response',
                     out.header = F, font.size = 'small',
                     column.sep.width = '5pt')

