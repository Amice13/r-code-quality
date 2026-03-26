## Replication code for 'Locked Out of College: ...'
## Jacob R. Brown and Hanno Hilbig (hhilbig@g.harvard.edu)
## Nov 18, 2020
##
## This file: Table B18, Figure B7

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

#### Define treatment variables 

treat_vars <- c('treat_felon', 'treat_black', 
                'treat_advocateWhite Advocate',
                'treat_advocateBlack Advocate')

#### Define function to plot results within subsets ####

pd <- position_dodge(0.6)

plot_function <- function(dataset, limits = c(-.1, 0.2, 0.05),
                          no_y_axis = F,
                          margins = F,
                          stack_legend = F,
                          title = '') {
  p2 <- ggplot(data = dataset, aes(x = var, y = estimate, group = controls)) + 
    geom_hline(yintercept = 0, linetype = 'dotted', size = 0.5,
               color = 'grey60') +
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
               size = 4) + 
    scale_shape_manual(name = '', values = c(21, 22)) +
    scale_color_manual(name = '', values = c('black', 'grey60')) +
    theme_bw() + 
    theme(legend.position = 'bottom') + 
    xlab('') + ylab('') + 
    coord_flip() + 
    scale_y_continuous(breaks = round(c(-.4, -.3, -.2, 
                                        -.1, 0, .1, .2, .3, .4), 3),
                       limits = c(limits[1], limits[2]))+
    theme(axis.title.x=element_blank())
  
  if (no_y_axis) {
    p2 <- p2 + theme(axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank())
  }
  
  ## Margins
  
  if (margins) {
    p2 <- p2 + theme(plot.margin = unit(c(0,0,0,5) * 0.1, "cm"))
  }
  
  ## Stack legend
  
  if (stack_legend) {
    p2 <- p2 + guides(shape=guide_legend(nrow=2,ncol =1),
                      color=guide_legend(nrow=2,ncol =1))
  }
  
  ## Title 
  
  if (!title == '') {
    p2 <- p2 + ggtitle(title) +
      theme(plot.title = element_text(hjust=0, margin=margin(0,0,10,0)))
  }
  
  p2
}

#### Estimate interaction models ####

m1_c <- lm(th_scale_agg ~ treat_felon + treat_black + treat_advocate
           + factor(covar_type) +
             covar_public_private + factor(covar_inst_size) + 
             factor(state), data = df)
i1_cv <- lm(th_scale_agg ~ treat_felon  + treat_black +
              treat_advocate + 
              treat_felon:covar_public_private + covar_type + 
              factor(covar_inst_size_raw) + 
              covar_public_private+  factor(state), 
            data = df)
i2_cv <- lm(th_scale_agg ~ treat_felon  + treat_black +
              treat_advocate +
              treat_black:covar_public_private + factor(covar_type) + 
              factor(covar_inst_size_raw) + 
              covar_public_private+ factor(state), 
            data = df)

## List of models for stargazer

table_list <- list(m1_c, i1_cv, i2_cv)

#### Table B18 : Treatment * school characteristics, thoroughness outcome ####

stargazer::stargazer(table_list,
                     keep = c(treat_vars, 
                              'covar_public_privatepublic',
                              'treat_felon:covar_public_privatepublic',
                              'treat_black:covar_public_privatepublic',
                              'Constant'), 
                     order = c(1, 3:5,2, 6,8,9,7, 10), 
                     covariate.labels = c('Criminal Record', 'Black', 
                                          'White Advocate', 'Black Advocate',
                                          'Public Institution (vs. Private)',
                                          'Criminal Record * Public Institution',
                                          'Black * Public Institution',
                                          'Intercept'),
                     add.lines = list(c('Covariates', rep('Yes', 3)),
                                      c('State FE', rep('Yes', 3))),
                     style = 'ajps', keep.stat = c('n', 'rsq'),
                     dep.var.labels = 'Thoroughness',
                     out.header = F, font.size = 'small',
                     column.sep.width = '5pt')

#### Estimate models, subsetting by school characteristics ####

## Private  / public

m1_pp <- lm(th_scale_agg ~ treat_felon + treat_black + treat_advocate
            + factor(covar_inst_size) + 
              factor(state) + factor(covar_type), data = df, 
            subset = df$covar_public_private == 'public')
m2_pp <- lm(th_scale_agg ~ treat_felon + treat_black + treat_advocate
            + factor(covar_inst_size) + 
              factor(state) + factor(covar_type), data = df, 
            subset = df$covar_public_private == 'private')

## Four year / two year 

m3_2y4y <- lm(th_scale_agg ~ treat_felon + treat_black +treat_advocate
              + factor(covar_inst_size) + 
                factor(state) + factor(covar_public_private), data = df, 
              subset = df$covar_type == 'two_year')
m4_2y4y <- lm(th_scale_agg ~ treat_felon + treat_black + treat_advocate
              + factor(covar_inst_size) + 
                factor(state) + factor(covar_public_private), data = df, 
              subset = df$covar_type == 'four_year')

## School size

m5_size <- lm(th_scale_agg ~ treat_felon + treat_black +treat_advocate
              + factor(covar_type) + 
                factor(state) + factor(covar_public_private), data = df, 
              subset = df$covar_inst_size == 'smaller_5k')
m6_size <- lm(th_scale_agg ~ treat_felon + treat_black +treat_advocate
              + factor(covar_type) + 
                factor(state) + factor(covar_public_private), data = df, 
              subset = df$covar_inst_size == 'bigger_5k')

#### Extract coefficients and CIs for plotting ####

## Private  / public

n <- c(length(m1_pp$residuals), length(m2_pp$residuals))

coefs_pp <- list(m1_pp, m2_pp) %>% 
  lapply(broom::tidy, conf.int = T) %>% 
  reduce(rbind) %>% 
  filter(str_detect(term, 'treat')) %>% 
  mutate(var = rep(c('Criminal Record', 'Black', 'White Advocate', 
                     'Black Advocate'), 2)) %>% 
  mutate(var = factor(var, levels = var[4:1])) %>% 
  mutate(controls = rep(c('Public college', 'Private college'),
                        each = 4)) %>% 
  mutate(n = rep(n, each = 4)) %>% 
  mutate(controls = paste0(controls, '\n(N = ', n, ')'))

## Four year / two year 

n <- c(length(m3_2y4y$residuals), length(m4_2y4y$residuals))

coefs_2y4y <- list(m3_2y4y, m4_2y4y) %>% 
  lapply(broom::tidy, conf.int = T) %>% 
  reduce(rbind) %>% 
  filter(str_detect(term, 'treat')) %>% 
  mutate(var = rep(c('Criminal Record', 'Black', 'White Advocate', 
                     'Black Advocate'), 2)) %>% 
  mutate(var = factor(var, levels = var[4:1])) %>% 
  mutate(controls = rep(c('Two-year college', 'Four-year college'),
                        each = 4)) %>% 
  mutate(n = rep(n, each = 4)) %>% 
  mutate(controls = paste0(controls, '\n(N = ', n, ')'))

## School size

n <- c(length(m5_size$residuals), length(m6_size$residuals))

coefs_size <- list(m5_size, m6_size) %>% 
  lapply(broom::tidy, conf.int = T) %>% 
  reduce(rbind) %>% 
  filter(str_detect(term, 'treat')) %>% 
  mutate(var = rep(c('Criminal Record', 
                     'Black', 
                     'White Advocate', 
                     'Black Advocate'), 2)) %>% 
  mutate(var = factor(var, levels = var[4:1])) %>% 
  mutate(controls = rep(c('< 5k students', '> 5k students'),
                        each = 4)) %>% 
  mutate(n = rep(n, each = 4)) %>% 
  mutate(controls = paste0(controls, '\n(N = ', n, ')'))

## Make plots

p1 <- plot_function(coefs_pp, limits = c(-2, 1.5, 0.1), margins = F, stack_legend = T,
                    title = 'Public / Private') +
  scale_y_continuous(breaks = round(seq(-2, 1.5, 0.5), 2))
p2 <- plot_function(coefs_2y4y, limits = c(-.4, .35, 0.1), no_y_axis = T, margins = T,
                    stack_legend = T, title = 'Two-year / Four-year') +
  scale_y_continuous(breaks = round(seq(-2, 1.5, 0.5), 2))
p3 <- plot_function(coefs_size, limits = c(-.4, .35, 0.1), no_y_axis = T, margins = T,
                    stack_legend = T, title = 'School Size') +
  scale_y_continuous(breaks = round(seq(-2, 1.5, 0.5), 2))

#### Figure B7 : Effects subset by three school covariates #### 

library(grid)
out_covars <- arrangeGrob(cbind(ggplotGrob(p1), 
                                ggplotGrob(p2), 
                                ggplotGrob(p3), size = "last"))

p1; p2; p3
