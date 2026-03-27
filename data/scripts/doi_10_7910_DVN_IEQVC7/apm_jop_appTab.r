### The Burden of Election Logistics
### Election Ballots and the Territorial Influence of Party Machines in Colombia

### The Journal of Politics, forthcoming.

### Santiago Alles  <santiago.alles@voxel-consulting.com>
### Monica Pachon   <mopachon@uniandes.edu.co>
### Manuela Munoz   <manuela.munozf@tamu.edu>

### updated: 04/20/2020


### Loading
### Packages

require(lme4)

require(stargazer)


### Appendix
### Table I-1

## Index data
G_idx_store %>% 
  filter(year >= year_min & year <= year_max) %>%
  filter( !department_code %in% dept_filter ) -> mod_dat

mod_dat$G_idx_log <- with(mod_dat, log(G_idx))


## model estimation: random intercepts by year and by department

lmer( G_idx_log ~ Gini_idx + ( 1 | department_code )  + ( 1 | year), data = mod_dat,
      control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')) ) -> mod1
lmer( G_idx_log ~ Gini_wt + ( 1 | department_code )  + ( 1 | year), data = mod_dat,
      control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')) ) -> mod2

lmer( G_idx ~ Gini_idx + ( 1 | department_code )  + ( 1 | year), data = mod_dat,
      control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')) ) -> mod3
lmer( G_idx ~ Gini_wt + ( 1 | department_code )  + ( 1 | year), data = mod_dat,
      control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')) ) -> mod4


## results table
stargazer( mod1, mod2, mod3, mod4, 
           title = paste( "Table I-1. Relation between Concentration Indexes: G-Index predicted by Gini & wt Gini (Colombia, ",
                          paste(paste(year_min, year_max, sep = '-'), ").", sep = ''),
                          "Linear mixed-effects models, with department- and year-level intercepts"),
           dep.var.labels = c('(log) G-Index', 'G-Index'),
           type = "text",
           no.space = T,
           digits = 3, star.cutoffs = c(.1, .05,.01,.001),
           column.labels = rep("dept fe + year fe", 4),
           model.numbers = T, model.names = F, object.names = F,
           omit.stat=c("f", "ser"),
           out = paste(mod_dir, "Table I-1.txt" , sep = '/')
           )



rm( mod_dat, 
    mod1, mod2, mod3, mod4)

