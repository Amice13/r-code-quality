## Replication code for 'Locked Out of College: ...'
## Jacob R. Brown and Hanno Hilbig (hhilbig@g.harvard.edu)
## Nov 18, 2020
##
## This file: Figure 1, Tables 2, B3, B4, B5, B7

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

pkgList <- c("tidyverse", "gridExtra")
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

#### Estimate base regressions ####

## Main models 

m1 <- lm(outcome_response ~ treat_felon + treat_black + treat_advocate, 
         data = df)
m1_c <- lm(outcome_response ~ treat_felon + treat_black + treat_advocate + 
             factor(covar_type) +
             factor(covar_public_private) + factor(covar_inst_size) + 
             factor(state), data = df)

## Subset: advocates

m1_ss <- lm(outcome_response ~ treat_felon + treat_black + treat_advocate, 
            data = df, subset = df$treat_reference == 1)
m1_c_ss <- lm(outcome_response ~ treat_felon + treat_black + treat_advocate + 
                factor(covar_type) +
                factor(covar_public_private) + factor(covar_inst_size) + 
                factor(state), data = df,
              subset = df$treat_reference == 1)

## Pooling black and white adovcates

m1_ss2 <- lm(outcome_response ~ treat_felon + treat_black + treat_reference, 
         data = df)
m1_c_ss2 <- lm(outcome_response ~ treat_felon + treat_black + treat_reference + 
             factor(covar_type) +
             factor(covar_public_private) + factor(covar_inst_size) + 
             factor(state), data = df)

#### Table B5: Main effects ####

stargazer::stargazer(list(m1, m1_c, m1_ss2,m1_c_ss2, m1_ss, m1_c_ss),
                      keep = "treat|constant", 
                     covariate.labels = c('Criminal Record', 'Black', 
                                         'White Advocate', 'Black Advocate',
                                       'Advocate (Pooled)',
                                        'Intercept'),
                     add.lines = list(c('Covariates', 'No', 'Yes', 'No', 'Yes',  'No', 'Yes'),
                                      c('State FE', 'No', 'Yes', 'No', 'Yes', 'No', 'Yes')),
                     style = 'ajps', keep.stat = c('n', 'rsq'),
                     dep.var.labels = 'Response',
                     out.header = F, font.size = 'small',
                     column.sep.width = '2pt')

## Define treatment vars

treat_vars <- c('treat_felon', 'treat_black', 
                'treat_advocateWhite Advocate',
                'treat_advocateBlack Advocate')

## DF for plotting

coefs_all <- list(m1, m1_c) %>% 
  lapply(broom::tidy, conf.int = T) %>% 
  reduce(rbind) %>% 
  filter(str_detect(term, "treat")) %>% 
  mutate(term = dplyr::recode(term,
                       `treat_felon` = 'Criminal Record',
                       `treat_black` = 'Black',
                       `treat_advocateWhite Advocate` = 'White Advocate',
                       `treat_advocateBlack Advocate` = 'Black Advocate')) %>% 
  mutate(term = factor(term, levels = unique(term)[4:1])) %>% 
  mutate(controls = rep(c('No controls', 'Controls'),
                        each = 4)) %>% 
  mutate(interaction = 'none',
         subset = 'none') %>% 
  mutate(outcome = 'response')

#### Figure 1 : Main effects ####

pd <- position_dodge(0.6)

p1 <- ggplot(data = coefs_all, aes(x = term, y = estimate, group = controls)) + 
  geom_hline(yintercept = 0, linetype = 'dotted', size = 0.5) +
  geom_errorbar(width = 0, aes(ymin = estimate - 1.96*std.error,
                               ymax = estimate + 1.96*std.error,
                               color = controls),
                position = pd) +
  geom_errorbar(width = 0, aes(ymin = estimate - qnorm(0.95)*std.error,
                               ymax = estimate + qnorm(0.95)*std.error,
                               color = controls),
                size = 1.1,
                position = pd) +
  geom_point(aes(shape = controls),
             position = pd, fill = 'white',
             size = 3) + 
  scale_shape_manual(name = '', values = c(21, 22)) +
  scale_color_manual(name = '', values = c('black', 'grey40')) +
  theme_bw() + 
  theme(legend.position = 'right') + 
  xlab('') + ylab('Treatment effect\n(Response Rate: 74.4%)') + 
  coord_flip() + 
  scale_y_continuous(breaks = round(seq(-0.2, 0.3, 0.05), 3),
                     limits = c(-0.1, 0.08))
p1

#### Estimate interaction models ####

i1 <- lm(outcome_response ~ treat_felon*treat_black + 
           treat_advocate, data = df)

i2 <- lm(outcome_response ~ treat_felon*treat_advocate + treat_black +
           treat_advocate, data = df)

i3 <- lm(outcome_response ~ treat_black*treat_advocate + treat_felon +
           treat_advocate, data = df)

## Add covars to each interaction

int_list <- list(i1, i2, i3)
int_list_covars <- lapply(int_list, function(x) {
  update(x, . ~ . + factor(covar_type) +
           factor(covar_public_private) + factor(covar_inst_size) + 
           factor(state))
})
int_list_full = c(int_list, int_list_covars)

#### Table B7: Interactions ####

table_list <- list(m1_c, int_list_covars[1], int_list_covars[2], 
                   int_list_covars[3])

stargazer::stargazer(table_list,
                     keep = c(treat_vars, 'treat_felon:treat_black',
                              'treat_felon:treat_advocateWhite Advocate',
                              'treat_felon:treat_advocateBlack Advocate',
                              'treat_black:treat_advocateWhite Advocate',
                              'treat_black:treat_advocateBlack Advocate',
                              'Constant'),
                     covariate.labels = c('Criminal Record',
                                          'Black',
                                          'White Advocate', 'Black Advocate',
                                          'Criminal Record * Black',
                                          'Criminal Record * White Advocate',
                                          'Criminal Record * Black Advocate',
                                          'Black * White Advocate',
                                          'Black * Black Advocate',
                                          'Intercept'),
                     order = c(1:10),
                     add.lines = list(c('Covariates', rep('Yes', 4)),
                                      c('State FE', rep('Yes', 4))),
                     style = 'ajps', keep.stat = c('n', 'rsq'),
                     dep.var.labels = 'Response',
                     out.header = F, font.size = 'small',
                     column.sep.width = '5pt')

## Prep for plotting

coefs_int <- int_list_full %>% 
  lapply(broom::tidy, conf.int = T) %>% 
  reduce(rbind) %>% 
  filter(str_detect(term, paste0(treat_vars, collapse = '|'))) %>% 
  mutate(term = dplyr::recode(term,
                       `treat_felon` = 'Criminal Record',
                       `treat_black` = 'Black',
                       `treat_advocateWhite Advocate` = 'White Advocate',
                       `treat_advocateBlack Advocate` = 'Black Advocate')) %>% 
  mutate(controls = rep(c('No Controls', 'Controls'), each = n() / 2)) %>% 
  mutate(interaction = ifelse(str_detect(term, ':'), 'yes', 'none')) %>% 
  mutate(subset = 'none') %>% 
  mutate(outcome = 'response')

#### Table B3: Diff in means / Non-parametric ####

treat_vars <- c('treat_felon', 'treat_black',
                'treat_advocate', 'treat_reference')

## Get the block variable

df$block <- paste0(df$covar_inst_size, 
                      df$covar_public_private,
                      df$covar_type)
df$block[df$block == 'bigger_5kprivatetwo_year'] <- 'bigger_5kprivatefour_year'

## Function to estimate diff in means

get_ss <- function(treat_var, treat = 1, control = 0) {
  
  ## Get treatment and control means 
  
  f <- as.formula(paste0('outcome_response ~ ', treat_var))
  est <- estimatr::difference_in_means(f, 
                                       data = df,
                                       blocks = block, 
                                condition1 = control,
                                condition2 = treat)
  est <- summary(est)$coefficients
  
  ## Number of obs
  nt <- sum(df[, treat_var] == treat)
  nc <- sum(df[, treat_var] == control)
  
  ## Out
  
  round(data.frame(est, nt, nc), 3)
}

## Run this

row1 <- get_ss(treat_var = 'treat_felon')
row2 <- get_ss(treat_var = 'treat_black')
row3 <- get_ss(treat_var = 'treat_advocate',
               treat = 'White Advocate', control = 'No Advocate')
row4 <- get_ss(treat_var = 'treat_advocate',
               treat = 'Black Advocate', control = 'No Advocate')
row5 <- get_ss(treat_var = 'treat_reference')

## To df

table_diffmeans <- do.call('rbind', list(row1, row2, row3, row4, row5))
table_diffmeans$DF <- NULL
table_diffmeans$CI.Lower <- NULL
table_diffmeans$CI.Upper <- NULL

## Get diff in means

out <- (lapply(c('treat_felon', 'treat_black', 'treat_advocate',
                 'treat_reference'),
       function(v) {
         df %>% group_by_(v) %>%
         summarise(gmean = round(mean(outcome_response, na.rm = T), 3)) %>%
         mutate(var = v) %>%
         data.frame() }
))


## Output

stargazer::stargazer(table_diffmeans, summary = F, rownames = F,
                     style = 'ajps')

#### Table 2 and B4: All treatment combinations ####

out_table <- df %>%
  group_by(treat_felon, treat_black, treat_advocate) %>%
  summarise(response_rate = mean(outcome_response, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(treat_black = as.character(treat_black),
         treat_felon = as.character(treat_felon)) %>%
  mutate(treat_felon = dplyr::recode(treat_felon,
                                      `1` = 'Criminal Record',
                                      `0` = 'No Criminal Record'),
         treat_black = dplyr::recode(treat_black,
                         `1` = 'Black',
                         `0` = 'White')) %>%
  data.frame()

## Output as Table 

stargazer::stargazer(out_table, style = 'ajps', 
                     rownames = F, summary = F)


