## Replication code for 'Locked Out of College: ...'
## Jacob R. Brown and Hanno Hilbig (hhilbig@g.harvard.edu)
## Nov 18, 2020
##
## This file: Figure B6, Tables B16, B17

## Set WD

rm(list = ls())
setwd("")

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

m1 <- lm(th_scale_agg ~ treat_felon + treat_black + treat_advocate, 
         data = df)
m1_c <- lm(th_scale_agg ~ treat_felon + treat_black + treat_advocate + 
             factor(covar_type) +
             factor(covar_public_private) + factor(covar_inst_size) + 
             factor(state), data = df)

## Subset: advocates

m1_ss <- lm(th_scale_agg ~ treat_felon + treat_black + treat_advocate, 
            data = df, subset = df$treat_reference == 1)
m1_c_ss <- lm(th_scale_agg ~ treat_felon + treat_black + treat_advocate + 
                factor(covar_type) +
                factor(covar_public_private) + factor(covar_inst_size) + 
                factor(state), data = df,
              subset = df$treat_reference == 1)

## Pooling black and white adovcates

m1_ss2 <- lm(th_scale_agg ~ treat_felon + treat_black + treat_reference, 
             data = df)
m1_c_ss2 <- lm(th_scale_agg ~ treat_felon + treat_black + treat_reference + 
                 factor(covar_type) +
                 factor(covar_public_private) + factor(covar_inst_size) + 
                 factor(state), data = df)

#### Table B16: Main effects ####

stargazer::stargazer(list(m1, m1_c, m1_ss2,m1_c_ss2, m1_ss, m1_c_ss),
                     keep = "treat|Constant", 
                     covariate.labels = c('Criminal Record', 'Black', 
                                          'White Advocate', 'Black Advocate',
                                          'Advocate (Pooled)',
                                          'Intercept'),
                     add.lines = list(c('Covariates', 'No', 'Yes', 'No', 'Yes',  'No', 'Yes'),
                                      c('State FE', 'No', 'Yes', 'No', 'Yes', 'No', 'Yes')),
                     style = 'ajps', keep.stat = c('n', 'rsq'),
                     dep.var.labels = 'Thoroughness',
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
         subset = 'none')

#### Figure B6 : Main effects ####

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
  xlab('') + ylab('Treatment effect') + 
  coord_flip()
p1

#### Estimate interaction models ####

i1 <- lm(th_scale_agg ~ treat_felon*treat_black + 
           treat_advocate, data = df)

i2 <- lm(th_scale_agg ~ treat_felon*treat_advocate + treat_black +
           treat_advocate, data = df)

i3 <- lm(th_scale_agg ~ treat_black*treat_advocate + treat_felon +
           treat_advocate, data = df)

## Add covars to each interaction

int_list <- list(i1, i2, i3)
int_list_covars <- lapply(int_list, function(x) {
  update(x, . ~ . + factor(covar_type) +
           factor(covar_public_private) + factor(covar_inst_size) + 
           factor(state))
})
int_list_full = c(int_list, int_list_covars)

#### Table B17: Interactions ####

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
                     dep.var.labels = 'Thoroughness',
                     out.header = F, font.size = 'small',
                     column.sep.width = '5pt')
