### The Burden of Election Logistics
### Election Ballots and the Territorial Influence of Party Machines in Colombia

### The Journal of Politics, forthcoming.

### Santiago Alles  <santiago.alles@voxel-consulting.com>
### Monica Pachon   <mopachon@uniandes.edu.co>
### Manuela Munoz   <manuela.munozf@tamu.edu>

### updated: 04/20/2020


### Loading
### Packages

require(arm)
require(ggplot2)

require(knitr)



### Part (i)
### Panels: Liberal & Conservative 

## Model Simulation
plot_sim <- sim(plot_mod, n.sims = n.sims)


## Prediction
## Frame

## Vars to predict
merge( merge( data.frame( reform = c(0,1) ), merge( data.frame( liberal = c(0,1)), data.frame( conserv = c(0,1)) ) %>% 
                filter( liberal + conserv <= 1 )),
       data.frame( vote_pct =seq( 0, 20, .25) ),
       all = T) %>% arrange(reform, -liberal, -conserv, vote_pct) %>% 
  tbl_df() -> pred

c('department_code', 'year') -> omit_vars
all.vars(formula(plot_mod))[-1] -> mod_vars
mod_vars[!mod_vars %in% colnames(pred)] -> mod_vars

unique(subset(pred, select = colnames(pred)[colnames(pred) %in% all.vars(formula(plot_mod))]) ) %>% 
  tbl_df() -> pred


## Control vars: median value
data.frame( pred, t(apply(as.matrix(subset(mod_data %>% filter(year >= p_year_min & year <= p_year_max ), 
                                           select = mod_vars )), 
                          2, quantile, probs=.5, na.rm=T))) -> X

## Interactions
with(X, vote_pct * reform) -> X$reform_votepct
with(X, liberal * reform) -> X$reform_lib
with(X, conserv * reform) -> X$reform_con


## Estimation Matrix
X <- subset( X, select = all.vars(formula(plot_mod))[-1])
X <- X[!names(X) %in% omit_vars]

X <- data.frame( 1, X) %>% tbl_df()

rm(mod_vars, omit_vars)


## Simulated
## Coefficients

b <- as.matrix(plot_sim@fixef)
Xb <- na.omit(t(as.matrix(X)%*% t(b)))

data.frame( pred, apply(Xb, 2, mean), t(apply(Xb, 2, quantile, probs=c(.025,.05,.95,.975)))) %>% tbl_df() -> plot_dat
colnames(plot_dat) <- c(colnames(pred), "mean", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")

## log model
with(plot_dat, exp(mean)) -> plot_dat$mean
with(plot_dat, exp(ci.lo.95)) -> plot_dat$ci.lo.95
with(plot_dat, exp(ci.lo.90)) -> plot_dat$ci.lo.90
with(plot_dat, exp(ci.up.90)) -> plot_dat$ci.up.90
with(plot_dat, exp(ci.up.95)) -> plot_dat$ci.up.95

## data labels
with(plot_dat, ifelse(reform == 1, '(new) Australian ballot', '(old) Party ballot')) -> plot_dat$reform_lab
with(plot_dat, ifelse(liberal == 1, 'Liberal Party', ifelse(conserv == 1, 'Conservative Party', 'Other Party'))) -> plot_dat$party_lab
with(plot_dat, ifelse(liberal == 1, 1, ifelse(conserv == 1, 2, 3))) -> plot_dat$party_pos


rm(X, Xb, b, pred)



### Part (ii)
### Panel: Complete Sample

## Model Simulation
drop_vars <- c("liberal", "conserv", "reform_lib", "reform_con", "department_code", "year")
plot_mod_all <- lmer( as.formula( paste( all.vars(formula(plot_mod))[1], 
                                         paste( paste( all.vars(formula(plot_mod))[-1][!all.vars(formula(plot_mod))[-1] %in% drop_vars ] ,
                                                       collapse = ' + ') ,
                                                '( 1 | department_code )', sep = ' + '), sep = ' ~ ')), 
                      data = mod_data %>% filter(year >= p_year_min & year <= p_year_max ) )

sim( plot_mod_all, n.sims = n.sims) -> plot_sim_all


## Prediction
## Frame

## Vars to predict
merge( data.frame( reform = c(0,1) ), data.frame( vote_pct =seq( 0, 20, .25) ),
       all = T) %>% arrange(reform, vote_pct) %>% 
  tbl_df() -> pred

c('department_code', 'year') -> omit_vars
all.vars(formula(plot_mod_all))[-1] -> mod_vars
mod_vars[!mod_vars %in% colnames(pred)] -> mod_vars

unique(subset(pred, select = colnames(pred)[colnames(pred) %in% all.vars(formula(plot_mod_all))]) ) %>% 
  tbl_df() -> pred


## Control vars: median value
data.frame( pred, t(apply(as.matrix(subset(mod_data %>% filter(year >= p_year_min & year <= p_year_max ), 
                                           select = mod_vars )), 
                          2, quantile, probs=.5, na.rm=T))) -> X

## Interactions
with(X, vote_pct * reform) -> X$reform_votepct


## Estimation Matrix
X <- subset( X, select = all.vars(formula(plot_mod_all))[-1])
X <- X[!names(X) %in% omit_vars]

X <- data.frame( 1, X) %>% tbl_df()

rm(drop_vars, plot_mod_all, mod_vars, omit_vars)


## Simulated
## Coefficients

b <- as.matrix(plot_sim_all@fixef)
Xb <- na.omit(t(as.matrix(X)%*% t(b)))

data.frame( pred, apply(Xb, 2, mean), t(apply(Xb, 2, quantile, probs=c(.025,.05,.95,.975)))) %>% tbl_df() -> plot_dat_all
colnames(plot_dat_all) <- c(colnames(pred), "mean", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")

## log model
with(plot_dat_all, exp(mean)) -> plot_dat_all$mean
with(plot_dat_all, exp(ci.lo.95)) -> plot_dat_all$ci.lo.95
with(plot_dat_all, exp(ci.lo.90)) -> plot_dat_all$ci.lo.90
with(plot_dat_all, exp(ci.up.90)) -> plot_dat_all$ci.up.90
with(plot_dat_all, exp(ci.up.95)) -> plot_dat_all$ci.up.95

## data labels
with(plot_dat_all, ifelse(reform == 1, '(new) Australian ballot', '(old) Party ballot')) -> plot_dat_all$reform_lab
'Complete Sample' -> plot_dat_all$party_lab

0 -> plot_dat_all$party_pos
NA -> plot_dat_all$liberal -> plot_dat_all$conserv

subset(plot_dat_all, select = c(colnames(plot_dat))) %>% 
  tbl_df() -> plot_dat_all

rm(X, Xb, b, pred)


### Combine
### Plot Data

rbind( plot_dat, plot_dat_all) %>% 
  arrange(party_pos, reform, vote_pct) %>% 
  tbl_df() -> plot_dat

plot_dat %>% filter(party_pos != 3) -> plot_dat           ### filter: drop 'Other Parties' from plot


rm(plot_dat_all, plot_sim_all, plot_sim)



### Rug
### Data

subset(mod_data %>% filter(year >= p_year_min & year <= p_year_max ), 
       select = c(G_idx, vote_pct, liberal, conserv, reform)) %>% 
  tbl_df() -> rug_dat
with(rug_dat, ifelse(liberal == 1, 'Liberal Party', ifelse(conserv == 1, 'Conservative Party', 'Other Party'))) -> rug_dat$party_lab
with(rug_dat, ifelse(reform == 1, '(new) Australian ballot', '(old) Party ballot')) -> rug_dat$reform_lab

with(rug_dat, ifelse(liberal == 1, 1, ifelse(conserv == 1, 2, 3))) -> rug_dat$party_pos

rbind( rug_dat, 
       subset( data.frame( subset(rug_dat, select = c(G_idx, vote_pct, reform, reform_lab)),
                           liberal = NA, conserv = NA, party_lab = 'Complete Sample', party_pos = 0),
               select = colnames(rug_dat))) %>% 
  tbl_df() -> rug_dat

rug_dat %>% filter(!is.na(G_idx) ) -> rug_dat             ### filter: drop missing obs
rug_dat %>% filter(party_pos != 3) -> rug_dat             ### filter: drop 'Other Parties' from plot



### Plot
### Script

# plot limits
x_max <- 20


# ggplot element
p <- ggplot( data = plot_dat %>% filter(vote_pct <= x_max), aes(x = vote_pct, y = mean ))


# Chart Region: light color
p <- p + theme_bw()
p <- p + theme(panel.border=element_rect(colour="white"))


# Grid Format
p <- p + theme(panel.grid.major.x=element_line(colour="#D0D0D0",size=.25),
               panel.grid.major.y=element_line(colour="#D0D0D0",size=.5),
               panel.grid.minor=element_blank())
p <- p + theme(axis.ticks=element_blank())


### Axis Text and Marks
p <- p + labs(y="Expected Vote Concentration: (log) G-Index\n", x="Votes (%)")

p <- p + theme(axis.text = element_text(size = 18, colour = "#535353", face="bold")) + 
  theme(axis.title = element_text(size = 20, colour = 'black', face="bold", vjust=.5))

p <- p + scale_y_continuous(limits = c(0, max(plot_dat$ci.up.95)), breaks = seq(0, 40, 5) )
p <- p + scale_x_continuous(limits = c(0, x_max), breaks = seq(0, x_max, length.out = 5) )

p <- p + geom_hline(yintercept = 0, size = 1.2, colour="#535353")

# plot legend
p <- p + theme( legend.background = element_rect(fill="#FFFFFF"),
                legend.text = element_text(size = 18, colour="#3C3C3C", face="bold" ),
                legend.title = element_blank(),
                legend.key = element_rect(fill="#FFFFFF", colour = NA),
                legend.position = c(0.885, 0.800))

# plot colors & shapes
p <- p + scale_colour_manual( values = c('red', 'black') )
p <- p + scale_size_manual( values = c(1.50, 0.75))

# content
p <- p + geom_ribbon(aes(ymin = ci.lo.95, ymax = ci.up.95, group = reform_lab ), alpha = .30)
p <- p + geom_line( aes(colour = reform_lab, size = reform_lab ))

p <- p + geom_rug( data = rug_dat %>% filter(vote_pct <= x_max), aes( x = vote_pct, y = 0 ), alpha = .5)

p <- p + facet_wrap( ~ party_lab ) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text = element_text(size = 24, colour = "black", face = "bold", vjust = .75))


# save
ggsave(paste(plot_dir, plot_file, sep = "/"), p, dpi = 800, width = 15, height = 7.5)



## Store
## Predictions

# save table
cat( paste( 'Figure III-1', "--------------", 
            'Model Predictions based on: Model 2, Table III-2', 
            'Linear mixed-effects model [lmer]. DV: log(G-Index)', sep = '\n'), '',
     rep('', 2),
     kable( plot_dat %>% 
              dplyr::select(reform_lab, party_lab, reform, vote_pct, mean, ci.lo.95, ci.lo.90, ci.up.90, ci.up.95 ) %>% 
              arrange(reform, party_lab, vote_pct) , 
            digits = 4, align = c(rep("l", 2), rep("r", 7) ) ), 
     sep = "\n",
     file = paste(plot_dir, 'Figure III-1 Predictions.txt', sep = '/') )




rm( p, plot_dat, rug_dat, x_max )


