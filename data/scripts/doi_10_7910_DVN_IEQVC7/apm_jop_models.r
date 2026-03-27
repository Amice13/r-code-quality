### The Burden of Election Logistics
### Election Ballots and the Territorial Influence of Party Machines in Colombia

### The Journal of Politics, forthcoming.

### Santiago Alles  <santiago.alles@voxel-consulting.com>
### Monica Pachon   <mopachon@uniandes.edu.co>
### Manuela Munoz   <manuela.munozf@tamu.edu>

### updated: 04/20/2020


### Loading
### Packages

require(betareg)
require(arm)

require(stargazer)




### Index
### Data

## Mod Data
G_idx_store %>% filter( year >= year_min & year <= year_max ) -> mod_data         ## filter years
mod_data %>% filter( !department_code %in% dept_filter ) -> mod_data              ## filter departments


## Variable corrections
mod_data$pop_density_log <- with(mod_data, log(pop_density))
mod_data$G_idx_log <- with(mod_data, log(G_idx))

## Interaction terms
with(mod_data, reform * liberal) -> mod_data$reform_lib
with(mod_data, reform * conserv) -> mod_data$reform_con
with(mod_data, vote_pct * reform) -> mod_data$reform_votepct

with(mod_data, y_total_adj_gini * y_total_adj_pc) -> mod_data$y_total_Gini



### Manuscript
### Equations

## Equations
eq_1_nF <- 'Gini_idx ~ reform + magnitude + liberal + conserv + vote_pct + n_mun + n_parties +
                reform_votepct +
                nbi + pop_density_log'

eq_2_nF <- 'Gini_idx ~ reform + magnitude + liberal + conserv + vote_pct + n_mun + n_parties +
                reform_lib + reform_con + reform_votepct +
                nbi + pop_density_log'

eq_1_wF <- paste( eq_1_nF, 'y_total_adj_pc + y_total_adj_gini + guerrillas_idx + paramilitary_idx', sep = ' + ' )
eq_2_wF <- paste( eq_2_nF, 'y_total_adj_pc + y_total_adj_gini + guerrillas_idx + paramilitary_idx', sep = ' + ' )



## models
mod1 <- betareg( as.formula(eq_1_wF) ,
                 link = "logit",
                 data = mod_data %>% filter(year %in% 1990:1991), 
                 method='Nelder-Mead' )

mod2 <- betareg( as.formula(eq_2_wF) ,
                 link = "logit",
                 data = mod_data %>% filter(year %in% 1990:1991),
                 method='Nelder-Mead' )

mod3 <- betareg( as.formula(paste(eq_2_wF, 'may_cycle', sep = '+')) ,
                 link = "logit",
                 data = mod_data %>% filter(year %in% 1986:1994),
                 method='Nelder-Mead' )

mod4 <- betareg( as.formula(paste(eq_2_nF, 'may_cycle', sep = '+')),
                 link = "logit",
                 data = mod_data,
                 method='Nelder-Mead' )


## results table
stargazer( mod1, mod2, mod3, mod4,
           title = paste( "Table 1. Effect of Ballot Reform on Concentration of Party Votes (Colombia, ",
                          paste(paste(year_min, year_max, sep = '-'), ").", sep = ''), 
                          "Beta Regression models"),
           dep.var.labels = rep( 'Gini Index', 4),
           type = "text",
           no.space = T,
           digits = 4, star.cutoffs = c(.1, .05,.01,.001),
           column.labels = c(rep('1990-1991', 2), '1986-1994', '1970-2002'), 
           model.numbers = T, model.names = F, object.names = F,
           omit.stat=c("f", "ser"),
           out = paste(mod_dir, "Table 1.txt" , sep = '/')
           )



### Figure 3:
### Expected Effect

plot_mod <- mod2                         ## set model

p_year_max <- 1991                       ## set plot's years: max
p_year_min <- 1990                       ## set plot's years: min

plot_file <- "Figure 3.png"

source(paste(dir, "apm_jop_predPlot_1.r", sep = '/' ))


rm( plot_mod, 
    p_year_max, p_year_min, 
    plot_file)




### Summary
### Statistics

## variables
summ_vars <- unique(c( 'Gini_idx', 'Gini_wt', 'G_idx_log', 'G_idx', 
                       all.vars(formula(mod1)), all.vars(formula(mod2)), all.vars(formula(mod3)), all.vars(formula(mod4)) ))
summ_vars <- summ_vars[!summ_vars %in% c('department_code', 'year')]
summ_vars <- summ_vars[!summ_vars %in% c('reform_lib', 'reform_con', 'reform_incumb', 'reform_votepct')]


## save table
stargazer( as.data.frame(mod_data %>% filter(year %in% 1990:1991) %>% dplyr::select(all_of(summ_vars))), 
           title = 'Table IV-1. Descriptive statistics (1990-1991)', 
           type = "text",
           no.space = T,
           digits = 4,
           summary.stat = c("n", "mean", "median", "min", "max", 'sd'),
           out = paste(mod_dir, "Table IV-1-A Summary Stats.txt" , sep = '/') 
           )

stargazer( as.data.frame(mod_data %>% filter(year %in% 1970:2002) %>% dplyr::select(all_of(summ_vars))), 
           title = 'Table IV-1. Descriptive statistics (1970-2002)', 
           type = "text",
           no.space = T,
           digits = 4,
           summary.stat = c("n", "mean", "median", "min", "max", 'sd'),
           out = paste(mod_dir, "Table IV-1-B Summary Stats.txt" , sep = '/')
           )


rm(summ_vars)



### Appendix III
### Equations

## Equations

# (1) DV's
dv_1 <- 'Gini_wt'
dv_2 <- 'G_idx_log'

# (2) IV's
eq_app_1_nF <- paste( all.vars(formula(eq_1_nF))[-1], collapse = ' + ' )
eq_app_2_nF <- paste( all.vars(formula(eq_2_nF))[-1], collapse = ' + ' )

eq_app_1_wF <- paste( all.vars(formula(eq_1_wF))[-1], collapse = ' + ' )
eq_app_2_wF <- paste( all.vars(formula(eq_2_wF))[-1], collapse = ' + ' )



## Table III-1 :: models
app_3.1_mod1 <- betareg( as.formula(paste(dv_1, eq_app_1_wF, sep = ' ~ ')) ,
                         link = "logit",
                         data = mod_data %>% filter(year %in% 1990:1991), 
                         method = 'Nelder-Mead' )

app_3.1_mod2 <- betareg( as.formula(paste(dv_1, eq_app_2_wF, sep = ' ~ ')) ,
                         link = "logit",
                         data = mod_data %>% filter(year %in% 1990:1991),
                         method = 'Nelder-Mead' )

app_3.1_mod3 <- betareg( as.formula(paste(paste(dv_1, eq_app_2_wF, sep = ' ~ '), 'may_cycle', sep = '+')) ,
                         link = "logit",
                         data = mod_data %>% filter(year %in% 1986:1994),
                         method = 'Nelder-Mead' )

app_3.1_mod4 <- betareg( as.formula(paste(paste(dv_1, eq_app_2_nF, sep = ' ~ '), 'may_cycle', sep = '+')),
                         link = "logit",
                         data = mod_data,
                         method = 'Nelder-Mead' )


##  Table III-1 :: results tables
stargazer( app_3.1_mod1, app_3.1_mod2, app_3.1_mod3, app_3.1_mod4,
           title = paste( "Table III-1. Effect of Ballot Reform on Concentration of Party Votes (Colombia, ",
                          paste(paste(year_min, year_max, sep = '-'), ").", sep = ''), 
                          "Beta Regression models"),
           dep.var.labels = rep('(weighted) Gini Idx', 4),
           type = "text",
           no.space = T,
           digits = 4, star.cutoffs = c(.1, .05,.01,.001),
           column.labels = c(rep('1990-1991', 2), '1986-1994', '1970-2002'), 
           model.numbers = T, model.names = F, object.names = F,
           omit.stat=c("f", "ser"),
           out = paste(mod_dir, "Table III-1.txt" , sep = '/')
           )




## Table III-2 :: models
app_3.2_mod1 <- lmer( as.formula(paste(dv_2, paste(eq_app_1_wF, '( 1 | department_code )', sep = '+'), sep = ' ~ '))  ,
                      data = mod_data %>% filter(year %in% 1990:1991),
                      control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')) )

app_3.2_mod2 <- lmer( as.formula(paste(dv_2, paste(eq_app_2_wF, '( 1 | department_code )', sep = '+'), sep = ' ~ '))  ,
                      data = mod_data %>% filter(year %in% 1990:1991),
                      control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')) )

app_3.2_mod3 <- lmer( as.formula(paste(dv_2, paste(eq_app_2_wF, 'may_cycle + ( 1 | department_code )', sep = '+'), sep = ' ~ '))  ,
                      data = mod_data %>% filter(year %in% 1986:1994),
                      control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')) )

app_3.2_mod4 <- lmer( as.formula(paste(dv_2, paste(eq_app_2_nF, 'may_cycle + ( 1 | department_code )', sep = '+'), sep = ' ~ '))  ,
                      data = mod_data,
                      control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')) )



##  Table III-2 :: results tables
stargazer( app_3.2_mod1, app_3.2_mod2, app_3.2_mod3, app_3.2_mod4,
           title = paste( "Table III-2. Effect of Ballot Reform on Concentration of Party Votes (Colombia, ",
                          paste(paste(year_min, year_max, sep = '-'), ").", sep = ''), 
                          "Linear mixed-effects models, with department-level intercepts"),
           dep.var.labels = rep('(log) G-Idx', 4),
           type = "text",
           no.space = T,
           digits = 4, star.cutoffs = c(.1, .05,.01,.001),
           column.labels = c(rep('1990-1991', 2), '1986-1994', '1970-2002'), 
           model.numbers = T, model.names = F, object.names = F,
           omit.stat=c("f", "ser"),
           out = paste(mod_dir, "Table III-2.txt" , sep = '/')
           )



## Table III-3 :: models
app_3.3_mod1 <- mod2
app_3.3_mod2 <- mod3
app_3.3_mod3 <- betareg( as.formula(paste(eq_2_wF, 'may_cycle', sep = '+')) ,
                         link = "logit",
                         data = mod_data  %>% filter(year %in% 1986:2002),
                         method = 'Nelder-Mead' )


##  Table III-3 :: results tables
stargazer( app_3.3_mod1, app_3.3_mod2, app_3.3_mod3,
           title = paste( "Table III-3. Effect of Ballot Reform on Concentration of Party Votes (Colombia, ",
                          paste(paste(year_min, year_max, sep = '-'), ").", sep = ''), 
                          "Beta Regression models"),
           dep.var.labels = rep('Gini Index', 3),
           type = "text",
           no.space = T,
           digits = 4, star.cutoffs = c(.1, .05,.01,.001),
           column.labels = c('1990-1991', '1986-1994', '1986-2002'), 
           model.numbers = T, model.names = F, object.names = F,
           omit.stat=c("f", "ser"),
           out = paste(mod_dir, "Table III-3.txt" , sep = '/')
           )



### Appendix-III Plot
### Expected Effect

plot_mod <- app_3.2_mod2                 ## set model
n.sims <- 50000                          ## set number of simulations

p_year_max <- 1991                       ## set plot's years: max
p_year_min <- 1990                       ## set plot's years: min

plot_file <- "Figure III-1.png"

source(paste(dir, "apm_jop_predPlot_2.r", sep = '/' ))


rm( plot_mod, n.sims, 
    p_year_max, p_year_min, 
    plot_file)





rm( app_3.1_mod1, app_3.1_mod2, app_3.1_mod3, app_3.1_mod4,
    app_3.2_mod1, app_3.2_mod2, app_3.2_mod3, app_3.2_mod4,
    app_3.3_mod1, app_3.3_mod2, app_3.3_mod3)

rm( dv_1, dv_2, 
    eq_app_1_nF, eq_app_1_wF, eq_app_2_nF, eq_app_2_wF)

