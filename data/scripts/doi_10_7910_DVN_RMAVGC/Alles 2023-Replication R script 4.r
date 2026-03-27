## Representación de Mujeres en la Cámara de Diputados:
## Instituciones, Partidos y Actitudes en Argentina, 1983-2023

## Replication Data for: 
## Section 4 :: 'La representación de intereses'

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

rep_data <- haven::read_dta()                        ## load "Alles 2023-Replication Data 04 Legis-level data.dta"




## Logit   -----------------------------------------------
## models

mod <- list()

# no weights
mod[['logit_mod_1']] <- glm( support_abortion ~ leg_ideology_D1 + 
                               female + 
                               religion + 
                               SOCD5 + single + kids + 
                               experienced ,
                             family = binomial( link = 'logit'),
                             data = rep_data)

mod[['logit_mod_2']] <- glm( support_abortion ~ ID101 + 
                               female + 
                               religion + 
                               SOCD5 + single + kids + 
                               experienced,
                             family = binomial( link = 'logit'),
                             data = rep_data)

mod[['logit_mod_3']] <- glm( support_abortion ~ fdt + ucr + pro + 
                               female + 
                               religion + 
                               SOCD5 + single + kids + 
                               experienced ,
                             family = binomial( link = 'logit'),
                             data = rep_data)


# store results
art_store[['models']] <- mod


rm(mod)



## Results   -----------------------------------------------
## Table App-5, in Appendix

stargazer::stargazer( art_store[['models']] ,
                      type = 'text', 
                      title = 'Support of Abortion Legalization: House 2022 (Legislator-level data)',
                      no.space = T,
                      digits = 4, 
                      star.cutoffs = c(.05, .01, .001),
                      # dep.var.labels = '',
                      model.numbers = T, 
                      model.names = T, 
                      object.names = F,
                      omit.stat = c("f", "ser")
                      )



## Figure   -----------------------------------------------
## data

# selected model
pred_mod <- art_store$models[['logit_mod_1']]

# model simulation
mod_sim <- arm::sim(pred_mod, n.sims = 50000)

# frame
data.frame( ID101 = median(rep_data$ID101, na.rm = T),
            SOCD5 = median(rep_data$SOCD5, na.rm = T), 
            single = 0, 
            kids = 1,
            lo_educ = 0, hi_educ = 0, 
            experienced = 0) %>%
  merge( data.frame( female = 0:1, 
                     gender = c('Hombres', 'Mujeres'))) %>%
  merge( rbind( 
    with( rep_data %>% 
            filter( religion == 1),
          data.frame( group = 'Cree en Dios',
                      religion = 1,
                      leg_ideology_D1 = seq( quantile( leg_ideology_D1, 
                                                       probs = .15, na.rm = T ),
                                             quantile( leg_ideology_D1, 
                                                       probs = .85, na.rm = T ), 
                                             length.out = 50 )) ),
    
    with( rep_data %>% 
            filter( religion == 0),
          data.frame( group = 'No cree',
                      religion = 0,
                      leg_ideology_D1 = seq( quantile( leg_ideology_D1, 
                                                       probs = .15, na.rm = T ),
                                             quantile( leg_ideology_D1, 
                                                       probs = .85, na.rm = T ), 
                                             length.out = 50 )) ) ) ) -> pred_frame

pred_frame %>%
  bind_rows( pred_frame %>%
               select(-leg_ideology_D1) %>%
               unique() %>% 
               merge( data.frame(leg_ideology_D1 = seq( -1.5, 1.5, .25)) )) %>%
  unique() %>%
  relocate( c('gender', 'group'),
            .before = ID101 ) %>%
  arrange( female, religion, leg_ideology_D1) %>%
  as_tibble() -> pred_frame


# sim coefficients
pred_frame %>% select( all_of((all.vars(formula(pred_mod))[-1]))) %>%
  data.frame( 1, .) -> X

b <- as.matrix( mod_sim@coef)
Xb <- t(as.matrix(X) %*% t(b))

# prediction
pred_frame %>%
  select( group, gender, leg_ideology_D1) %>%
  data.frame( .,
              fit = apply(exp(Xb) / (1+exp(Xb)), 2, median) * 100,
              t(apply(exp(Xb) / (1+exp(Xb)), 2, quantile, probs = c(.05, .95))) * 100 ) %>%
  as_tibble() %>%
  rename( lwr = X5. ) %>%
  rename( upr = X95.) -> plot_data


rm(pred_frame, X, b, Xb)



## Figure   -----------------------------------------------
## data

# titles
plot_title <- '\nProbabilidad esperada de apoyar (fuertemente) la legalización del aborto'
plot_subtitle <- 'Cámara de Diputados: Argentina, 2022\n'

# plot.caption <- ''

# axis
y_lim <- c(0, 100)
y_bks <- seq(0, 100, length.out = 5)

x_lim <- c( -2.75, 2.75)
x_bks <- c( x_lim[1], x_lim[2], seq( -2., 2, length.out = 5)) %>% sort()
x_lab <- c('Izq.', x_bks[ 2:(length(x_bks)-1)], 'Der.')

# median positions
rbind( rep_data %>% 
         filter( female == 1) %>% 
         with(., data.frame( female = 1,
                             med_pos = median(leg_ideology_D1, na.rm = T) ) ),
       rep_data %>% 
         filter( female == 0) %>% 
         with(., data.frame( female = 0,
                             med_pos = median(leg_ideology_D1, na.rm = T) ) )) %>%
  merge( data.frame( female = 0:1, gender = c('Hombres', 'Mujeres')),
         by = 'female') %>%
  as_tibble() -> plot_bks

# text labels
plot_data %>% 
  select( group, gender) %>% 
  filter( (group %in% 'Cree en Dios' & gender %in% 'Hombres') |
            (!group %in% 'Cree en Dios' & !gender %in% 'Hombres') ) %>% 
  unique() %>% 
  left_join( rbind( plot_data %>% 
                      filter( group %in% 'Cree en Dios') %>% 
                      filter( gender %in% 'Hombres' ) %>%
                      group_by( group, gender) %>%
                      summarise( xpos = min(leg_ideology_D1, na.rm = T),
                                 ypos = max(fit)),
                    plot_data %>% 
                      filter(!group %in% 'Cree en Dios') %>% 
                      filter(!gender %in% 'Hombres' ) %>%
                      group_by( group, gender) %>%
                      summarise( xpos = max(leg_ideology_D1, na.rm = T),
                                 ypos = min(fit)) ),
             by = c('group', 'gender')) %>%
  mutate( xpos = ifelse( xpos < 0, xpos - .1, xpos + .1)) %>%
  mutate( hjust = ifelse( xpos < 0, 1, 0)) %>% 
  mutate( lab = group) -> plot_txt

# plot rug
rep_data %>% 
  select( female, religion, leg_ideology_D1) %>% 
  na.omit() %>%
  merge( data.frame( female = 0:1, gender = c('Hombres', 'Mujeres')),
         by = 'female') %>%
  merge( data.frame( group = c('No cree', 'Cree en Dios'), religion = 0:1),
         by = 'religion') %>%
  as_tibble() %>%
  select(group, gender, female, religion, leg_ideology_D1) %>%
  arrange( female, religion, leg_ideology_D1) -> plot_rug



## Figure   -----------------------------------------------
## script

# element
p <- ggplot( data = plot_data, aes(x = leg_ideology_D1, y = fit, group = group ) )

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
p <- p + scale_x_continuous( limits = x_lim, breaks = x_bks, labels = x_lab)

# plot title
p <- p + labs( y = 'Probabilidad esperada\n')

p <- p + 
  # ggtitle( plot_title, subtitle = plot_subtitle ) + 
  theme( plot.title = element_text(face = "bold", colour = "black", size = 17.5, lineheight = .75), 
         plot.title.position = 'panel' ) +
  theme( plot.subtitle = element_text(face = "bold", colour = "#3C3C3C", size = 16, lineheight = .75))

# plot caption
# p <- p + labs( caption = plot.caption) + theme( plot.caption = element_text(size = 10, colour = "#6C6C6C", face = "bold"))

# no legend
p <- p + theme(legend.position = "none")


# median position
p <- p + geom_vline( data = plot_bks,
                     aes( xintercept = med_pos),
                     linewidth = .6, colour="#535353", linetype = "dotted")

p <- p + geom_text( data = plot_txt, 
                    aes( y = ypos, x = xpos, label = group, hjust = hjust ),
                    size = 4, fontface = 'bold', colour = "black" )

p <- p + geom_label( data = plot_bks,
                     aes( y = 14, x = med_pos ), 
                     label = 'M', size = 4, fontface = "bold", 
                     colour = 'white', fill = 'black',
                     inherit.aes = F)

# x-axis
p <- p + geom_hline(yintercept = 0, linewidth = 1.2, colour="#535353")

# scale colours
p <- p + scale_color_manual( values = c('blue', 'red'))

# plot content
p <- p + geom_rug( data = plot_rug, 
                   aes( x = leg_ideology_D1), 
                   inherit.aes = F,
                   alpha = .7, linewidth = 1)

p <- p + geom_ribbon( aes(ymax = upr, ymin = lwr ), alpha = .15)
p <- p + geom_line( aes(colour = group), linewidth = 1 )

p <- p + facet_wrap( vars(gender), ncol = 2) +
  theme(strip.background = element_blank()) + 
  theme(strip.text = element_text(size = 13, colour = "black", face = "bold", vjust = .75))



## store   -----------------------------------------------
## figure

art_store[['Fig-6']] <- p

# aspect ratio
# dpi = 800, width = 12.75, height = 4


rm( y_lim, y_bks,
    x_lim, x_bks, x_lab,
    plot_bks, plot_txt,
    plot_rug,
    mod_sim, pred_mod,
    plot_data, 
    plot_title, plot_subtitle,
    p )



## Figure   -----------------------------------------------
## data

# data
rep_data %>%
  select( party, female, SOCD5, fdt, pro, ucr, VAL103) %>%
  filter(!is.na(VAL103)) -> plot_data

# titles
plot_title <- '\nPosición ante la Legalización del Aborto'
plot_subtitle <- 'Cámara de Diputados: Argentina, 2022\n'

plot.caption <- 'Fuente: PELA (Univ. de Salamanca)'

# axis
x_lim <- c( .5, 10.5)
x_bks <- 1:10
x_lab <- c('Desaprueba\nfirmemente', x_bks[ 2:(length(x_bks)-1)], 'Aprueba\nfirmemente')

y_lim <- c(0, 70)



## Figure   -----------------------------------------------
## script

# element
p <- ggplot( data = plot_data, aes(VAL103 ) )

# chart region
p <- p + theme_bw()
p <- p + theme(panel.border = element_rect(colour = "white"))

# grid
p <- p + theme(panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(colour = "#D0D0D0", linewidth = .50),
               panel.grid.minor.x = element_blank(),
               panel.grid.minor.y = element_line(colour = "#D0D0D0", linewidth = .05))
p <- p + theme(axis.ticks = element_blank())

# axis
p <- p +
  # theme(axis.title = element_blank()) + 
  theme(axis.title.x = element_blank() )  + 
  theme(axis.title.y = element_text(size = 14, colour = "#535353", face = "bold"))  + 
  theme(axis.text.x = element_text(size = 12, colour = "#535353", face = "bold"))  + 
  theme(axis.text.y = element_text(size = 12, colour = "#535353", face = "bold")) 

p <- p + scale_y_continuous( limits = y_lim)
p <- p + scale_x_continuous( limits = x_lim, breaks = x_bks, labels = x_lab )

# plot title
p <- p + labs( y = 'Frecuencia (n)\n')

p <- p + 
  ggtitle( plot_title, subtitle = plot_subtitle ) + 
  theme( plot.title = element_text(face = "bold", colour = "black", size = 17.5, lineheight = .75), 
         plot.title.position = 'panel' ) +
  theme( plot.subtitle = element_text(face = "bold", colour = "#3C3C3C", size = 16, lineheight = .75))

# plot caption
p <- p + 
  labs( caption = plot.caption) +
  theme( plot.caption = element_text(size = 10, colour = "#6C6C6C", face = "bold"))

# no legend
p <- p + theme(legend.position = "none")

# plot content
p <- p + geom_bar( fill = "#D0D0D0")

# x-axis
p <- p + geom_hline( yintercept = 0, linewidth = 1.2, colour="#535353")



## store   -----------------------------------------------
## figure

art_store[['App Fig-2']] <- p

# aspect ratio
# dpi = 800, width = 12.75, height = 4


rm( plot_data,
    plot_title, plot_subtitle, 
    plot.caption,
    y_lim, x_lim, x_bks, x_lab,
    p )



