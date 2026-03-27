### The Burden of Election Logistics
### Election Ballots and the Territorial Influence of Party Machines in Colombia

### The Journal of Politics, forthcoming.

### Santiago Alles  <santiago.alles@voxel-consulting.com>
### Monica Pachon   <mopachon@uniandes.edu.co>
### Manuela Munoz   <manuela.munozf@tamu.edu>

### updated: 04/20/2020


### Loading
### Packages

require(emmeans)

require(cowplot)
require(ggplot2)

require(knitr)



### Prediction (i)
### Party Models

## Prediction
## Frame

## model vars
all.vars(formula(plot_mod))[-1] -> mod_vars

## Vars to predict
merge( merge( data.frame( reform = c(0,1) ), merge( data.frame( liberal = c(0,1)), data.frame( conserv = c(0,1)) ) %>% 
                filter( liberal + conserv <= 1 )),
       data.frame( vote_pct = seq( 0, 20, .25) ),
       all = T) %>% arrange(reform, -liberal, -conserv, vote_pct) %>% 
  tbl_df() -> pred

pred <- pred %>% filter(liberal == 1 | conserv == 1)

## Control vars: median value
data.frame( pred, t(apply( as.matrix(model.frame(plot_mod)), 2, quantile, probs=.5, na.rm=T))) %>% 
  dplyr::select(all_of(mod_vars)) %>% 
  tbl_df() -> X

## Interactions
with(X, vote_pct * reform) -> X$reform_votepct
with(X, liberal * reform) -> X$reform_lib
with(X, conserv * reform) -> X$reform_con


## pred store
store <- data.frame(matrix(nrow = nrow(pred), ncol = 5))
colnames(store) <- c('fit', "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")


## Model
## Prediction

for (i in 1:nrow(store)) {
  
  # set level = .90
  emmeans::emm_options(ref_grid = list(level = .90))
  
  # preditions :: c.i. 90%
  loop_pred <- emmeans::emmeans( plot_mod, specs = ~ reform + liberal + conserv + reform_lib + reform_con + vote_pct + reform_votepct, 
                                 nesting = NULL, data = X[i, ] )
  
  loop_pred <- data.frame(loop_pred)
  
  store[i, "ci.lo.90"] <- loop_pred$asymp.LCL
  store[i, "ci.up.90"] <- loop_pred$asymp.UCL
  
  
  # set level = .95
  emmeans::emm_options(ref_grid = list(level = .95))
  
  # preditions :: c.i. 95%
  loop_pred <- emmeans::emmeans( plot_mod, specs = ~ reform + liberal + conserv + reform_lib + reform_con + vote_pct + reform_votepct, 
                                 nesting = NULL, data = X[i, ] )
  
  loop_pred <- data.frame(loop_pred)
  
  store[i, "ci.lo.95"] <- loop_pred$asymp.LCL
  store[i, "ci.up.95"] <- loop_pred$asymp.UCL
  
  
  # preditions :: mean
  if('response' %in% colnames(loop_pred)) store[i, 'fit'] <- loop_pred$response else loop_pred$emmean -> store[i, 'fit']
  
  rm(loop_pred)

  
}


## Store
## Results

## data labels
with(pred, ifelse(reform == 1, '(new) Australian ballot', '(old) Party ballot')) -> pred$reform_lab
with(pred, ifelse(liberal == 1, 'Liberal Party', ifelse(conserv == 1, 'Conservative Party', 'Other Party'))) -> pred$party_lab
with(pred, ifelse(liberal == 1, 1, ifelse(conserv == 1, 2, 3))) -> pred$party_pos


pred_1 <- data.frame( pred, store) %>% tbl_df()


rm( i, X, pred, store)




### Prediction (ii)
### Full-Sample Models

## Model
## estimation

drop_vars <- c("liberal", "conserv", "reform_lib", "reform_con", "department_code", "year")

plot_mod_all <- betareg( as.formula( paste( all.vars(formula(plot_mod))[1], 
                                            paste( all.vars(formula(plot_mod))[-1][!all.vars(formula(plot_mod))[-1] %in% drop_vars ] ,
                                                   collapse = ' + '), sep = ' ~ ')) ,
                         data = mod_data %>% filter(year %in% p_year_min:p_year_max), 
                         method='Nelder-Mead' )

rm(drop_vars)


## Prediction
## Frame

## model vars
all.vars(formula(plot_mod_all))[-1] -> mod_vars

## Vars to predict
merge( data.frame( reform = c(0,1) ), data.frame( vote_pct =seq( 0, 20, .25) ),
       all = T) %>% arrange(reform, vote_pct) %>% 
  tbl_df() -> pred

## Control vars: median value
data.frame( pred, t(apply( as.matrix(model.frame(plot_mod_all)), 2, quantile, probs=.5, na.rm=T))) %>% 
  dplyr::select(all_of(mod_vars)) %>% 
  tbl_df() -> X

## Interactions
with(X, vote_pct * reform) -> X$reform_votepct


## pred store
store <- data.frame(matrix(nrow = nrow(pred), ncol = 5))
colnames(store) <- c('fit', "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")



## Model
## Prediction

for (i in 1:nrow(store)) {

  # set level = .90
  emmeans::emm_options(ref_grid = list(level = .90))
  
  # preditions :: c.i. 90%
  loop_pred <- emmeans::emmeans( plot_mod_all, specs = ~ reform + vote_pct + reform_votepct, 
                                 nesting = NULL, data = X[i, ] )
  
  loop_pred <- data.frame(loop_pred)
  
  store[i, "ci.lo.90"] <- loop_pred$asymp.LCL
  store[i, "ci.up.90"] <- loop_pred$asymp.UCL
  
  
  # set level = .95
  emmeans::emm_options(ref_grid = list(level = .95))
  
  # preditions :: c.i. 95%
  loop_pred <- emmeans::emmeans( plot_mod_all, specs = ~ reform + vote_pct + reform_votepct, 
                                 nesting = NULL, data = X[i, ] )
  
  loop_pred <- data.frame(loop_pred)
  
  store[i, "ci.lo.95"] <- loop_pred$asymp.LCL
  store[i, "ci.up.95"] <- loop_pred$asymp.UCL
  
  
  # preditions :: mean
  if('response' %in% colnames(loop_pred)) store[i, 'fit'] <- loop_pred$response else loop_pred$emmean -> store[i, 'fit']
  
  rm(loop_pred)

  
}


rm(plot_mod_all)


## Store
## Results

## data labels
pred$reform_lab <- with(pred, ifelse(reform == 1, '(new) Australian ballot', '(old) Party ballot'))
pred$party_lab <- 'Complete Sample'

pred$party_pos <- 0
pred$liberal <- pred$conserv <- NA


pred_2 <- data.frame( pred, store) %>% tbl_df() %>% dplyr::select(all_of(colnames(pred_1)))


rm( i, X, pred, store, mod_vars)



### Combine
### Plot Data

rbind( pred_1, pred_2) %>% 
  arrange(party_pos, reform, vote_pct) %>% 
  tbl_df() -> plot_dat

plot_dat %>% filter(party_pos != 3) -> plot_dat           ### filter: drop 'Other Parties' from plot

# Map parameters
model_base <- (plot_dat %>% filter(party_pos == 0 & vote_pct == 3 & reform == 0))$fit 
model_pred <- (plot_dat %>% filter(party_pos == 0 & vote_pct == 3 & reform == 1))$fit 


rm(pred_1, pred_2)



### Rug
### Data

mod_data %>% 
  filter(year %in% p_year_min:p_year_max ) %>% 
  dplyr::select(Gini_idx, vote_pct, liberal, conserv, reform) %>% 
  tbl_df() -> rug_dat

with(rug_dat, ifelse(liberal == 1, 'Liberal Party', ifelse(conserv == 1, 'Conservative Party', 'Other Party'))) -> rug_dat$party_lab
with(rug_dat, ifelse(reform == 1, '(new) Australian ballot', '(old) Party ballot')) -> rug_dat$reform_lab

with(rug_dat, ifelse(liberal == 1, 1, ifelse(conserv == 1, 2, 3))) -> rug_dat$party_pos

rbind( rug_dat, 
       data.frame( rug_dat %>% dplyr::select(Gini_idx, vote_pct, reform, reform_lab),
                   liberal = NA, conserv = NA, party_lab = 'Complete Sample', party_pos = 0) %>% 
         dplyr::select(all_of(colnames(rug_dat))) ) %>% 
  tbl_df() -> rug_dat

rug_dat %>% filter(!is.na(Gini_idx) ) -> rug_dat          ### filter: drop missing obs
rug_dat %>% filter(party_pos != 3) -> rug_dat             ### filter: drop 'Other Parties' from plot



### Plot
### Script

# plot limits
x_max <- 20


# ggplot element
p <- ggplot( data = plot_dat %>% filter(vote_pct <= x_max) , aes(x = vote_pct, y = fit ))

# chart region
p <- p + theme_bw()
p <- p + theme(panel.border=element_rect(colour="white"))

# grid
p <- p + theme(panel.grid.major.x=element_line(colour="#D0D0D0",size=.25),
               panel.grid.major.y=element_line(colour="#D0D0D0",size=.5),
               panel.grid.minor=element_blank())
p <- p + theme(axis.ticks=element_blank())

# axis & marks
p <- p + labs(y="Expected Vote Concentration:\nGini Index\n", x="Votes (%)")

p <- p + theme(axis.text = element_text(size = 18, colour = "#535353", face="bold")) + 
  theme(axis.title = element_text(size = 20, colour = 'black', face="bold", vjust=.5))

p <- p + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, length=6) )
p <- p + scale_x_continuous(limits = c(0 - 1, x_max + 1), breaks = seq(0, x_max, length=5) )

p <- p + geom_hline(yintercept = 0, size = 1.2, colour="#535353")

# plot legend
p <- p + theme( legend.background = element_rect(fill="#FFFFFF"),
                legend.text = element_text(size = 18, colour="#3C3C3C", face="bold" ),
                legend.title = element_blank(),
                legend.key = element_rect(fill="#FFFFFF", colour = NA),
                legend.position = c(0.850, 0.138))

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
cat( paste('Figure 3', "--------------", 
           'Model Predictions based on: Model 2, Table 1', 
           'Beta regression model [betareg]. DV: Gini Index', sep = '\n'), '',
     rep('', 2),
     kable( plot_dat %>% 
              dplyr::select(reform_lab, party_lab, reform, vote_pct, fit, ci.lo.95, ci.lo.90, ci.up.90, ci.up.95 ) %>% 
              arrange(reform, party_lab, vote_pct) , 
            digits = 4, align = c(rep("l", 2), rep("r", 7) ) ), 
     sep = "\n",
     file = paste(plot_dir, 'Figure 3 Predictions.txt', sep = '/') )




rm( p, plot_dat, rug_dat, x_max)

