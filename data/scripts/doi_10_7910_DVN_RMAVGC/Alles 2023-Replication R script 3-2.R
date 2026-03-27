## Representación de Mujeres en la Cámara de Diputados:
## Instituciones, Partidos y Actitudes en Argentina, 1983-2023

## Replication Data for: 
## Section 3 :: 'El lugar de los partidos'

## Santiago Alles
## Departamento de Ciencias Sociales
## Universidad de San Andrés
## <salles@udesa.edu.ar>


## updated: 2023-10-12


## Loading
## Packages

require(dplyr)
require(ggplot2)



rm(list = ls())



art_store <- list()


## Load   -----------------------------------------------
## data

rep_data <- haven::read_dta()                        ## load "Alles 2023-Replication Data 03-2 Cand-level data.dta"




## Logit   -----------------------------------------------
## models

mod <- list()

mod[['logit_mod_1']]  <- glm( female_leg ~ 
                                pj + ucr + jxc + fit + ps + 
                                parity +
                                magnitude + 
                                female_gov + turnout ,
                              family = binomial( link = 'logit'),
                              data = rep_data )

mod[['logit_mod_2']]  <- glm( female_leg ~ 
                                pj + ucr + jxc + fit + ps + 
                                parity +
                                magnitude + 
                                female_gov + turnout +
                                dist_quota_1s  ,
                              family = binomial( link = 'logit'),
                              data = rep_data )

mod[['logit_mod_3']]  <- glm( female_leg ~ 
                                pj + ucr + jxc + fit + ps +
                                parity +
                                senate_cycle + pres_election +
                                magnitude + 
                                female_gov + turnout + 
                                dist_quota_1s ,
                              family = binomial( link = 'logit'),
                              data = rep_data )


# store results
art_store[['models']] <- mod


rm(mod)



## Results   -----------------------------------------------
## Table App-4, in Appendix

stargazer::stargazer( art_store[['models']] ,
                      title = "Predicting a Woman at the Top of the Ticket, House 1983-2021 (Party-level data)",
                      dep.var.labels = 'Woman at the Top',
                      type = "text",
                      no.space = T,
                      digits = 4, star.cutoffs = c( .05, .01, .001), 
                      model.numbers = T, model.names = F, object.names = F,
                      omit.stat = c("f", "ser")
                      )



## Figure   -----------------------------------------------
## data

# selected model
pred_mod <- art_store$models[['logit_mod_2']]

# model simulation
mod_sim <- arm::sim(pred_mod , n.sims = 50000)

# prelim
with( rep_data , 
      data.frame( parity = 1 , 
                  magnitude = median(magnitude), 
                  female_gov = 0, 
                  turnout = median(turnout),
                  dist_quota_1s = 1,
                  senate_cycle = 1, 
                  pres_election = 1) ) -> basic_vars


# controls were fixed at:
# magnitude = 3
# turnout = 77.74201


# frame
data.frame( label = c('Frente de Izquierda\ny Trabajadores', 
                      'Frente para la Victoria\nFrente de Todos',
                      'Otros\npartidos', 
                      'Partido\nSocialista',
                      'Unión Cívica\nRadical', 
                      'Juntos por\nel Cambio', 
                      'PRO'),
            fit = c(1, 0, 0, 0, 0, 0, 0), pj = c(0, 1, 0, 0, 0, 0, 0), 
            ps = c(0, 0, 0, 1, 0, 0, 0), ucr = c(0, 0, 0, 0, 1, 0, 0), 
            jxc = c(0, 0, 0, 0, 0, 1, 0), pro = c(0, 0, 0, 0, 0, 0, 1)) %>%
  mutate( rank = 1:nrow(.)) %>%
  mutate( label = forcats::fct_reorder( label, rank)) %>%
  select( -rank) -> pred_frame

pred_frame %>% 
  left_join( basic_vars %>%
               merge( rep_data %>% select( pj, ucr, jxc, fit, ps) %>% 
                        unique()) %>%
               select( all_of( all.vars(formula(pred_mod))[-1])) ,
             by = c('pj', 'ucr', 'jxc', 'fit', 'ps')) -> pred_frame

# sim coefficients
pred_frame %>% select( all_of((all.vars(formula(pred_mod))[-1]))) %>%
  data.frame( 1, .) -> X

b <- as.matrix( mod_sim@coef)
Xb <- t(as.matrix(X) %*% t(b))

# prediction
pred_frame %>%
  data.frame( .,
              pred = apply(exp(Xb) / (1+exp(Xb)), 2, median) * 100,
              t(apply(exp(Xb) / (1+exp(Xb)), 2, quantile, probs = c(.05, .95))) * 100 ) %>%
  as_tibble() %>%
  rename( lwr = X5. ) %>%
  rename( upr = X95.) %>%
  select( label, pred, lwr, upr) %>%
  rename( fit = pred) %>%
  mutate( lab = round(fit, 1) %>%
            ifelse( . %in% 0:100, paste(., 0, sep = '.'), .) %>%
            paste( ., '%', sep = '' ) ) -> plot_data

# text
plot_data %>% 
  mutate( lab = round(fit, 1) %>%
            ifelse( . %in% 0:100, paste(., 0, sep = '.'), .) %>%
            paste( ., '%', sep = '' ) ) %>%
  mutate( xpos = label) %>%
  mutate( ypos = upr + 2) %>% 
  select( xpos, ypos, lab) -> plot_text


rm(pred_frame, X, b, Xb)



## Average   -----------------------------------------------
## Probability

(all.vars(formula(pred_mod))[-1][!all.vars(formula(pred_mod))[-1] %in% c('pj', 'ucr', 'jxc', 'fit', 'pro', 'ps')] %>%
   paste( ., collapse = ' + ') %>%
   paste( all.vars(formula(pred_mod))[1], ., sep = ' ~ ') %>%
   as.formula() %>%
   glm( . ,
        family = binomial( link = 'logit'),
        data = rep_data ) %>%
   predict.glm( ., newdata = basic_vars, type = 'link') %>%
   plogis() * 100) %>%
  round( ., 2) %>% paste(., '%', sep = '') %>%
  paste( 'Average Probability:', .)



## Plot   -----------------------------------------------
## Specs

# titles
plot.title <- '\nProbabilidad que una Mujer sea Cabeza de Lista'
plot.subtitle <- 'Cámara de Diputados: Argentina, 2011-2021\n'

# plot.caption <- ''

# axis
y_lim <- c(0, 67.5)
y_bks <- seq(0, 60, length.out = 5)



## Figure   -----------------------------------------------
## script

# element
p <- ggplot( data = plot_data %>%
               filter(!label %in% 'PRO' ), 
             aes( x = label, y = fit, ymin = lwr, ymax = upr ) )

# chart region
p <- p + theme_bw()
p <- p + theme(panel.border = element_rect(colour = "white"))

# grid
p <- p + theme(panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(colour = "#D0D0D0", linewidth = .50),
               panel.grid.minor = element_blank())
p <- p + theme(axis.ticks = element_blank())

# axis
p <- p +
  # theme(axis.title = element_blank()) + 
  theme(axis.title.x = element_blank() )  + 
  theme(axis.title.y = element_text(size = 14, colour = "#535353", face = "bold"))  + 
  theme(axis.text.x = element_text(size = 12, colour = "#535353", face = "bold"))  + 
  theme(axis.text.y = element_text(size = 12, colour = "#535353", face = "bold")) 

p <- p + scale_y_continuous( limits = y_lim, breaks = y_bks)

# plot title
p <- p + labs( y = 'Probabilidad esperada\n')

# plot title
p <- p + 
  #  ggtitle( plot.title, subtitle = plot.subtitle ) + 
  theme( plot.title = element_text(face = "bold", colour = "black", size = 16.5, lineheight = .75), plot.title.position = 'panel' ) +
  theme( plot.subtitle = element_text(face = "bold", colour = "#3C3C3C", size = 16, lineheight = .75))

# plot caption
# p <- p + labs( caption = plot.caption) + theme( plot.caption = element_text(size = 10, colour = "#6C6C6C", face = "bold"))

# no legend
p <- p + theme(legend.position = "none")

# x-axis
p <- p + geom_hline(yintercept = 0, linewidth = 1.2, colour="#535353")

# plot content
p <- p + geom_pointrange( aes( ymin = lwr, ymax = upr) )
p <- p + geom_point( size = 2.5, colour = 'black', fill = '#D0D0D0', stroke = .75, pch = 21)

p <- p + geom_label( data = plot_text %>%
                       filter(!xpos %in% 'PRO' ),
                     inherit.aes = F,
                     aes( x = xpos, y = ypos, label = lab ), 
                     size = 4.25, label.size = NA )




## store   -----------------------------------------------
## figure

art_store[['Fig-5']] <- p

# aspect ratio
# dpi = 800, width = 12.75, height = 4




rm( mod_sim, pred_mod,
    basic_vars, plot_data, plot_text,
    plot.title, plot.subtitle,
    y_lim, y_bks,
    p )
