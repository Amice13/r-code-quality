## Representación de Mujeres en la Cámara de Diputados:
## Instituciones, Partidos y Actitudes en Argentina, 1983-2023

## Replication Data for: 
## Section 2 :: 'El efecto de las instituciones'

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

rep_data <- haven::read_dta()                        ## load "Alles 2023-Replication Data 02 Prov-level data.dta"




## Figure   -----------------------------------------------
## data

# data
rep_data %>%
  filter( election_type %in% 'HOUSE') %>%
  group_by( election_type, year) %>%
  summarise( m_seats = sum(m_seats),
             f_seats = sum(f_seats),
             x_seats = sum(x_seats) ) %>%
  mutate( x_seats = ifelse( x_seats < 0, 0, x_seats ) ) %>%
  mutate( seats = m_seats + f_seats + x_seats) %>%
  relocate( seats, .after = year) %>%
  mutate( m_seats_pct = m_seats / seats * 100 ) %>%
  mutate( f_seats_pct = f_seats / seats * 100 ) %>%
  mutate( x_seats_pct = x_seats / seats * 100 ) %>% 
  mutate( quota = ifelse( year %in% 1993:1999, 'Ley de Cupos', 'Sin cuotas')) %>% 
  mutate( quota = ifelse( year %in% 2001:2017, 'Decreto 1246/00', quota)) %>% 
  mutate( quota = ifelse( year > 2017, 'Ley de Paridad', quota)) -> plot_data

# text
plot_data %>% 
  group_by(quota) %>%
  summarise( term_st = min(year),
             term_end = max(year),
             f_avg = mean(f_seats_pct)) %>% 
  arrange( term_st) %>% 
  mutate( rank = 1:nrow(.)) %>%
  mutate( quota = forcats::fct_reorder( quota, rank)) %>%
  select( -rank) %>%
  left_join( plot_data %>% as.data.frame() %>% 
               select( year, f_seats_pct) %>%
               rename( f_st = f_seats_pct),
             by = c('term_st' = 'year')) %>%
  left_join( plot_data %>% as.data.frame() %>% 
               select( year, f_seats_pct) %>%
               rename( f_end = f_seats_pct),
             by = c('term_end' = 'year')) %>%
  mutate( text_st = paste( f_st %>% round( ., 1), '%', sep = '' )) %>%
  mutate( text_end = paste( f_end %>% round( ., 1), '%', sep = '' )) %>%
  mutate( pos_st = ifelse( term_st == 1983, f_st, f_st + 1 ) ) %>%
  mutate( pos_end = ifelse( term_st %in% c(1983, 2019), f_end, f_end - 1.75 ) ) -> plot_text

# add label's order
plot_data %>% 
  left_join( plot_text %>% 
               arrange( term_st) %>% 
               mutate( rank = 1:nrow(plot_text)) %>% 
               select( quota, rank),
             by = 'quota') %>%
  mutate( quota = forcats::fct_reorder( quota, rank)) %>%
  select( -rank) -> plot_data




## Plot   -----------------------------------------------
## Specs

# titles
plot.title <- '\nEvolución del Número de Mujeres Electas'
plot.subtitle <- 'Cámara de Diputados: Argentina, 1983-2021\n'

# plot.caption <- ''

# axis
x_lim <- c(1983 - 2.25, 2023 + .75)
y_lim <- c(0, 50)

x_bks <- seq(1983, 2023, 4)
y_bks <- seq(0, 45, length.out = 4)



## Figure   -----------------------------------------------
## script

# element
p <- ggplot( data = plot_text )

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
  theme(axis.title = element_blank()) + 
  theme(axis.text.x = element_text(size = 11, colour = "#535353", face = "bold"))  + 
  theme(axis.text.y = element_text(size = 11, colour = "#535353", face = "bold")) 

p <- p + scale_x_continuous( limits = x_lim, breaks = x_bks)
p <- p + scale_y_continuous( limits = y_lim, breaks = y_bks)

# no legend
p <- p + theme(legend.position = "none")

# x-axis
p <- p + geom_hline(yintercept = 0, linewidth = 1.2, colour="#535353")

# plot content
p <- p + geom_label( aes( x = term_st - .35, y = pos_st, label = text_st ), 
                     size = 3.85, hjust = 1, label.size = NA )
p <- p + geom_label( aes( x = term_end + .35, y = pos_end, label = text_end ), 
                     size = 3.85, hjust = 0, label.size = NA )

# p <- p + geom_segment( aes(x = term_st - .5, xend = term_end + .5, y = f_avg, yend = f_avg ), linetype = 'dotted')

p <- p + geom_line( data = plot_data %>% 
                      as.data.frame() %>%
                      select( year, f_seats_pct), 
                    aes(x = year, y = f_seats_pct),
                    linewidth = .65, colour = 'darkgrey')

p <- p + geom_line( data = plot_data, 
                    aes(x = year, y = f_seats_pct),
                    linewidth = 1.25, colour = 'black')

p <- p + geom_point( aes(x = term_st, y = f_st), 
                     size = 3, colour = 'black', fill = '#D0D0D0', stroke = .75, pch = 21)
p <- p + geom_point( aes(x = term_end, y = f_end), 
                     size = 3, colour = 'black', fill = '#D0D0D0', stroke = .75, pch = 21)


p <- p + facet_wrap( vars(quota) ) +
  theme(strip.background = element_blank()) + 
  theme(strip.text = element_text(size = 13, colour = "black", face = "bold", vjust = .75))




## store   -----------------------------------------------
## figure

art_store[['Fig-1']] <- p

# aspect ratio
# dpi = 800, width = 10.5, height = 6



rm( plot_data, plot_text,
    plot.title, plot.subtitle,
    x_lim, y_lim, x_bks, y_bks,
    p )



## Linear   -----------------------------------------------
## models

mod <- list()

mod[['OLS_mod_1']] <- lm( f_seat_pct ~ quota + str_quota + parity + 
                            magnitude + I(magnitude^2) + 
                            female_gov + turnout ,
                          data = rep_data)

mod[['OLS_mod_2']] <- lm( f_seat_pct ~ quota + str_quota + parity + 
                            magnitude + I(magnitude^2) + 
                            female_gov + turnout + 
                            enp_votes + enp_seats +
                            enp_votes * enp_seats +
                            magnitude * enp_seats ,
                          data = rep_data)

mod[['OLS_mod_3']] <- lm( f_seat_pct ~ quota + str_quota + parity + 
                            magnitude + I(magnitude^2) + 
                            pj_gov + ucr_gov + 
                            female_gov + turnout + 
                            enp_votes + enp_seats +
                            enp_votes * enp_seats +
                            magnitude * enp_seats ,
                          data = rep_data)

mod[['OLS_mod_4']] <- lm( f_seat_pct ~                   + parity + 
                            magnitude + I(magnitude^2) + 
                            pj_gov + ucr_gov + 
                            female_gov + turnout + 
                            enp_votes + enp_seats + 
                            IDG + prov_GDP_growth +
                            enp_votes * enp_seats +
                            magnitude * enp_seats ,
                          data = rep_data)


# store results
art_store[['models']] <- mod


rm(mod)



## Results   -----------------------------------------------
## Table App-1, in Appendix

stargazer::stargazer( art_store[['models']] , 
                      title = "Predicting the Election of Women in Legislative Seats, House 1983-2021 (Prov-level data)",
                      dep.var.labels = '% Women',
                      type = "text",
                      no.space = T,
                      digits = 4, star.cutoffs = c(.05, .01, .001), 
                      model.numbers = T, model.names = F, object.names = F,
                      omit.stat = c("f", "ser")
                      )





## Model   -----------------------------------------------
## Prediction

with( rep_data,
      data.frame( group = c('Sin cuotas', 'Ley de Cupos', 'Decreto 1246/00', 'Ley de Paridad'), 
                  quota = c( 0, 1, 0, 0), str_quota = c(0, 0, 1, 0), parity = c(0, 0, 0, 1),
                  magnitude = median(magnitude),
                  female_gov = 0, 
                  turnout = median(turnout), 
                  enp_votes = median(enp_votes),
                  enp_seats = median(enp_seats) ) ) %>%
  data.frame( ., predict.lm(  object = art_store$models[['OLS_mod_2']], 
                              newdata = ., interval = 'confidence' )) %>%
  as_tibble() %>% 
  arrange( fit) %>% 
  mutate( rank = 1:nrow(.)) %>%
  mutate( group = forcats::fct_reorder( group, rank)) %>%
  select( group, fit, lwr, upr ) %>% 
  mutate( lab = round(fit, 1) %>%
            ifelse( . %in% 0:90, paste(., 0, sep = '.'), .) %>%
            paste( ., '%', sep = '' ) ) -> plot_data


# controls were fixed at:
# magnitude = 3
# turnout = 77.51028
# enp_votes = 2.855597
# enp_seats = 1.8
 


## Plot   -----------------------------------------------
## Specs

# titles
plot.title <- '\nsegún Diseño de las Cuotas'
plot.subtitle <- 'Cámara de Diputados: Argentina, 1983-2021\n'

# plot.caption <- ''

# axis
y_lim <- c(0, 50)
y_bks <- seq(0, 45, length.out = 4)



## Figure   -----------------------------------------------
## script

# element
p <- ggplot( data = plot_data, aes(x = group, y = fit) )

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

p <- p + labs( y = 'Porcentaje esperado (%)\n')

p <- p + scale_y_continuous( limits = y_lim, breaks = y_bks)

# plot title
p <- p + ggtitle( plot.title ) + 
  theme( plot.title = element_text(face = "bold", colour = "black", size = 17.5, lineheight = .75), plot.title.position = 'panel' ) +
  theme( plot.subtitle = element_text(face = "bold", colour = "#3C3C3C", size = 16, lineheight = .75))

# no legend
p <- p + theme(legend.position = "none")

# x-axis
p <- p + geom_hline(yintercept = 0, linewidth = 1.2, colour="#535353")

# plot content
p <- p + geom_pointrange( aes( ymin = lwr, ymax = upr) )
p <- p + geom_point( size = 2.5, colour = 'black', fill = '#D0D0D0', stroke = .75, pch = 21)

p <- p + geom_label( aes( y = upr + 2, label = lab ), 
                     size = 4.25, label.size = NA)


# store
panel_1 <- p



rm( plot_data,
    plot.title, plot.subtitle,
    y_lim, y_bks,
    p )




## Model   -----------------------------------------------
## Prediction

# data
with( rep_data,
      data.frame( rbind( data.frame( group = 'Baja fragmentación', magnitude = 3:9, enp_seats = 1.8),
                         data.frame( group = 'Alta fragmentación', magnitude = 3:9, enp_seats = 3  ) ),
                  quota = 0, str_quota = 1, parity = 0,
                  female_gov = 0, 
                  turnout = median(turnout), 
                  enp_votes = median(enp_votes) ) ) %>%
  data.frame( ., predict.lm(  object = art_store$models[['OLS_mod_2']], newdata = ., interval = 'confidence' )) %>%
  as_tibble() %>%
  select( group, magnitude, fit, lwr, upr ) %>% 
  mutate( lab = round(fit, 1) %>%
            ifelse( . %in% 0:50, paste(., 0, sep = '.'), .) %>%
            paste( ., '%', sep = '' ) ) -> plot_data


# controls were fixed at:
# turnout = 77.51028
# enp_votes = 2.855597



# text
plot_data %>% 
  filter( magnitude == 9) %>% 
  select(group, fit) %>% unique() %>% 
  mutate( group = gsub( pattern = ' ', replacement = '\n', x = group) ) %>%
  rename( lab = group) %>%
  rename( ypos = fit) %>%
  mutate( xpos = 9.25) -> plot_text_1

plot_data %>% 
  filter( magnitude %in% seq(3, 9, 3)) %>%
  rename( xpos = magnitude) %>%
  mutate( xpos = ifelse( group %in% 'Baja fragmentación', xpos + .1, xpos ) ) %>%
  mutate( ypos = ifelse( stringr::str_detect( string = group, pattern = 'Baja'),
                         upr + 2, lwr - 2)) %>% 
  select( group, lab, ypos, xpos) -> plot_text_2



## Plot   -----------------------------------------------
## Specs

# titles
plot.title <- '\nsegún Magnitud de Distrito'
plot.subtitle <- 'Cámara de Diputados: Argentina, 1983-2021\n'

# plot.caption <- ''

# axis
x_lim <- c(3 - .25, 10.75)
y_lim <- c(0, 50)

x_bks <- seq(3, 9, 1)
y_bks <- seq(0, 45, length.out = 4)



## Figure   -----------------------------------------------
## script

# element
p <- ggplot( data = plot_data, aes(x = magnitude, y = fit, group = group) )

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
  theme(axis.title = element_blank()) + 
  theme(axis.text.x = element_text(size = 12, colour = "#535353", face = "bold"))  + 
  theme(axis.text.y = element_text(size = 12, colour = "#535353", face = "bold")) 

p <- p + scale_x_continuous( limits = x_lim, breaks = x_bks)
p <- p + scale_y_continuous( limits = y_lim, breaks = y_bks, position = "right")

# plot title
p <- p + ggtitle( plot.title ) +
  theme( plot.title = element_text(face = "bold", colour = "black", size = 17.5, lineheight = .75), plot.title.position = 'panel' ) +
  theme( plot.subtitle = element_text(face = "bold", colour = "#3C3C3C", size = 16, lineheight = .75))

# no legend
p <- p + theme(legend.position = "none")
p <- p + scale_fill_manual( values = c('white', '#D0D0D0'))

# x-axis
p <- p + geom_hline(yintercept = 0, linewidth = 1.2, colour="#535353")

# plot content
p <- p + geom_line( aes(group = group), colour = "#D0D0D0", linewidth = .25, linetype = "dashed")
p <- p + geom_pointrange( aes( ymin = lwr, ymax = upr), 
                          position = position_dodge(width = 0.30))

p <- p + geom_point( position = position_dodge(width = 0.30), 
                     size = 2.5, colour = 'black', fill = '#D0D0D0', stroke = .75, pch = 21)

p <- p + geom_label( data = plot_text_1, inherit.aes = F,
                     aes( x = xpos, y = ypos, label = lab ), 
                     size = 4.25, fontface = 'bold',
                     hjust = 0, label.size = NA, lineheight = 0.85 )

p <- p + geom_label( data = plot_text_2, inherit.aes = F,
                     aes( y = ypos, x = xpos, label = lab ),
                     size = 4.25, label.size = NA)


# store
panel_2 <- p



rm( plot_data, 
    plot_text_1, plot_text_2,
    plot.title, plot.subtitle,
    x_lim, y_lim, x_bks, y_bks,
    p )





## store   -----------------------------------------------
## figure

art_store[['Fig-2']] <- cowplot::plot_grid( panel_1, panel_2, ncol = 2 )


# aspect ratio
# dpi = 800,  width = 12.75, height = 4


rm( panel_1, panel_2 )





