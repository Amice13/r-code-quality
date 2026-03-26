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

rep_data <- haven::read_dta()                        ## load "Alles 2023-Replication Data 03-1 Party-level data.dta"




## Binomial   -----------------------------------------------
## models

mod <- list()

mod[['Binomial_mod_1']] <- glm( cbind(f_seats, m_seats) ~ quota + str_quota + parity + 
                                  seats + I(seats^2) + magnitude + 
                                  female_gov + turnout +
                                  pj + ucr + jxc,
                                family = binomial, 
                                data = rep_data )

mod[['Binomial_mod_2']] <- glm( cbind(f_seats, m_seats) ~ quota + str_quota + parity + 
                                  seats + I(seats^2) + magnitude + 
                                  female_gov + turnout +
                                  pj + ucr + jxc + 
                                  enp_votes + enp_seats,
                                family = binomial, 
                                data = rep_data )

mod[['Binomial_mod_3']] <- glm( cbind(f_seats, m_seats) ~ quota + str_quota + parity + 
                                  seats + I(seats^2) + magnitude + 
                                  female_gov + turnout +
                                  pj + ucr + jxc + 
                                  enp_votes + enp_seats +
                                  enp_votes * enp_seats +
                                  seats * enp_seats ,
                                family = binomial, 
                                data = rep_data )

mod[['Binomial_mod_4']] <- glm( cbind(f_seats, m_seats) ~ quota + str_quota + parity + 
                                  seats + I(seats^2) + magnitude + 
                                  pj_gov + ucr_gov + 
                                  female_gov + turnout +
                                  pj + ucr + jxc + 
                                  enp_votes + enp_seats +
                                  enp_votes * enp_seats +
                                  seats * enp_seats ,
                                family = binomial, 
                                data = rep_data )


# store results
art_store[['models']] <- mod


rm(mod)



## Results   -----------------------------------------------
## Table App-2, in Appendix

stargazer::stargazer( art_store[['models']]  ,
                      title = "Predicting the Election of Women in Legislative Seats, House 1983-2021 (Party-level data)",
                      dep.var.labels = '% Women',
                      type = "text",
                      no.space = T,
                      digits = 4, star.cutoffs = c(.05, .01, .001), 
                      model.numbers = T, model.names = F, object.names = F,
                      omit.stat = c("f", "ser")
                      )




## Figure   -----------------------------------------------
## data

# data
with( rep_data,
      data.frame( quota = 0, str_quota = 1, parity = 0,
                  seats = 3, 
                  magnitude = 3:9,
                  female_gov = 0, 
                  turnout = median(turnout),
                  pj = 0, ucr = 0, jxc = 0, 
                  enp_votes = median(enp_votes),
                  enp_seats = median(enp_seats) ) ) %>%
  data.frame( ., predict.glm( art_store$mod[['Binomial_mod_3']], newdata = ., type = 'link',  se.fit = T) ) %>% as_tibble() %>%
  mutate( lwr = plogis(fit - se.fit * 1.96) * 100 ) %>%
  mutate( upr = plogis(fit + se.fit * 1.96) * 100 ) %>%
  mutate( fit = plogis(fit) * 100) %>% 
  select( magnitude, seats, fit, lwr, upr ) %>% 
  mutate( lab = round(fit, 1) %>%
            ifelse( . %in% 0:100, paste(., 0, sep = '.'), .) %>%
            paste( ., '%', sep = '' ) ) -> plot_data


# controls were fixed at:
# turnout = 77.51028
# enp_votes = 2.855597
# enp_seats = 1.8


# text
plot_data %>%
  filter( magnitude %in% c(3, 9)) %>%
  mutate( ypos = ifelse( fit == max(fit), upr + 2, lwr - 2 ))%>%
  mutate( xpos = magnitude) -> plot_text


## Plot   -----------------------------------------------
## Specs

# titles
plot.title <- '\nsegún Magnitud del Distrito'
plot.subtitle <- 'Cámara de Diputados: Argentina, 1983-2021\n'

# plot.caption <- ''

# axis
x_lim <- c(2.5, 9.5)
y_lim <- c(0, 57.5)

x_bks <- 1:9
y_bks <- seq(0, 45, length.out = 4)



## Figure   -----------------------------------------------
## script

# element
p <- ggplot( data = plot_data, aes(x = magnitude, y = fit) )

# chart region
p <- p + theme_bw()
p <- p + theme(panel.border = element_rect(colour = "white"))

# grid
p <- p + theme(panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(colour = "#D0D0D0", size = .50),
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

# plot caption
# p <- p + labs( caption = plot.caption) + theme( plot.caption = element_text(size = 10, colour = "#6C6C6C", face = "bold"))

# no legend
p <- p + theme(legend.position = "none")
p <- p + scale_fill_manual( values = c('white', '#D0D0D0'))
# x-axis
p <- p + geom_hline(yintercept = 0, linewidth = 1.2, colour="#535353")

# plot content
p <- p + geom_line( colour = "#D0D0D0", linewidth = .25, linetype = "dashed")
p <- p + geom_pointrange( aes( ymin = lwr, ymax = upr) )
p <- p + geom_point( size = 2.5, colour = 'black', fill = '#D0D0D0', stroke = .75, pch = 21)

p <- p + geom_label( data = plot_text,
                     aes( y = ypos, x = xpos, label = lab ), 
                     size = 4.25, label.size = NA)


# store
panel_1 <- p


rm( plot_data, plot_text,
    plot.title, plot.subtitle,
    x_lim, y_lim, x_bks, y_bks,
    p )



## Figure   -----------------------------------------------
## data

# data
with( rep_data,
      data.frame( quota = 0, str_quota = 1, parity = 0,
                  seats = 1:6, 
                  magnitude = median(magnitude),
                  female_gov = 0, 
                  turnout = median(turnout),
                  pj = 0, ucr = 0, jxc = 0, 
                  enp_votes = median(enp_votes),
                  enp_seats = median(enp_seats) ) ) %>%
  data.frame( ., predict.glm( art_store$mod[['Binomial_mod_3']], newdata = ., type = 'link',  se.fit = T) ) %>% as_tibble() %>%
  mutate( lwr = plogis(fit - se.fit * 1.96) * 100 ) %>%
  mutate( upr = plogis(fit + se.fit * 1.96) * 100 ) %>%
  mutate( fit = plogis(fit) * 100) %>% 
  select( magnitude, seats, fit, lwr, upr ) %>% 
  mutate( lab = round(fit, 1) %>%
            ifelse( . %in% 0:50, paste(., 0, sep = '.'), .) %>%
            paste( ., '%', sep = '' ) ) -> plot_data

# controls were fixed at:
# magnitude = 3
# turnout = 77.51028
# enp_votes = 2.855597
# enp_seats = 1.8


# text
plot_data %>%
  filter( seats %in% c(1, 6)) %>%
  mutate( ypos = ifelse( fit == max(fit), upr + 1.75, lwr - 1.75 ))%>%
  mutate( xpos = seats) -> plot_text



## Plot   -----------------------------------------------
## Specs

# titles
plot.title <- '\nsegún Magnitud del Partido'
plot.subtitle <- 'Cámara de Diputados: Argentina, 1983-2021\n'

# plot.caption <- ''

# axis
x_lim <- c(0.5, 6.5)
y_lim <- c(0, 57.5)

x_bks <- 1:9
y_bks <- seq(0, 45, length.out = 4)



## Figure   -----------------------------------------------
## script

# element
p <- ggplot( data = plot_data, aes(x = seats, y = fit) )

# chart region
p <- p + theme_bw()
p <- p + theme(panel.border = element_rect(colour = "white"))

# grid
p <- p + theme(panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(colour = "#D0D0D0", size = .50),
               panel.grid.minor = element_blank())
p <- p + theme(axis.ticks = element_blank())

# axis
p <- p +
  # theme(axis.title = element_blank()) + 
  theme(axis.title.x = element_blank() )  + 
  theme(axis.title.y = element_text(size = 14, colour = "#535353", face = "bold"))  + 
  theme(axis.text.x = element_text(size = 12, colour = "#535353", face = "bold"))  + 
  theme(axis.text.y = element_text(size = 12, colour = "#535353", face = "bold")) 

p <- p + labs( y = 'Probabilidad esperada\n')

p <- p + scale_x_continuous( limits = x_lim, breaks = x_bks)
p <- p + scale_y_continuous( limits = y_lim, breaks = y_bks)

# plot title
p <- p + ggtitle( plot.title ) +
  theme( plot.title = element_text(face = "bold", colour = "black", size = 17.5, lineheight = .75), plot.title.position = 'panel' ) +
  theme( plot.subtitle = element_text(face = "bold", colour = "#3C3C3C", size = 16, lineheight = .75))

# plot caption
# p <- p + labs( caption = plot.caption) + theme( plot.caption = element_text(size = 10, colour = "#6C6C6C", face = "bold"))

# no legend
p <- p + theme(legend.position = "none")
p <- p + scale_fill_manual( values = c('white', '#D0D0D0'))
# x-axis
p <- p + geom_hline(yintercept = 0, linewidth = 1.2, colour="#535353")

# plot content
p <- p + geom_line( colour = "#D0D0D0", linewidth = .25, linetype = "dashed")
p <- p + geom_pointrange( aes( ymin = lwr, ymax = upr) )
p <- p + geom_point( size = 2.5, colour = 'black', fill = '#D0D0D0', stroke = .75, pch = 21)

p <- p + geom_label( data = plot_text,
                     aes( y = ypos, x = xpos, label = lab ), 
                     size = 4.25, label.size = NA)


# store
panel_2 <- p


rm( plot_data, plot_text,
    plot.title, plot.subtitle,
    x_lim, y_lim, x_bks, y_bks,
    p )



## store   -----------------------------------------------
## figure

art_store[['Fig-3']] <- cowplot::plot_grid( panel_2, panel_1, ncol = 2 ) 

# aspect ratio
# dpi = 800, width = 12.75, height = 4


rm( panel_2, panel_1)




## Figure   -----------------------------------------------
## data

with( rep_data,
      data.frame( quota = 0, str_quota = 1, parity = 0,
                  seats = 3, 
                  magnitude = median(magnitude),
                  female_gov = 0, 
                  turnout = median(turnout),
                  data.frame( group = c('Partido\nJusticialista', 'Unión Cívica\nRadical', 
                                        'Juntos por\nel Cambio', 'Otros\npartidos'), 
                              pj = c(1, 0, 0, 0), 
                              ucr = c(0, 1, 0, 0), 
                              jxc = c(0, 0, 1, 0),
                              rank = c( 1, 3, 4, 2)) %>%
                    mutate( group = forcats::fct_reorder( group, rank)) , 
                  enp_votes = median(enp_votes),
                  enp_seats = median(enp_seats) ) ) %>%
  data.frame( ., predict.glm( art_store$mod[['Binomial_mod_3']], newdata = ., type = 'link',  se.fit = T) ) %>% as_tibble() %>%
  mutate( lwr = plogis(fit - se.fit * 1.96) * 100 ) %>%
  mutate( upr = plogis(fit + se.fit * 1.96) * 100 ) %>%
  mutate( fit = plogis(fit) * 100) %>% 
  select( group, fit, lwr, upr ) %>% 
  mutate( lab = round(fit, 1) %>%
            ifelse( . %in% 0:50, paste(., 0, sep = '.'), .) %>%
            paste( ., '%', sep = '' ) ) -> plot_data


# controls were fixed at:
# magnitude = 3
# turnout = 77.51028
# enp_votes = 2.855597 
# enp_seats = 1.8



## Plot   -----------------------------------------------
## Specs

# titles
plot.title <- '\nProbabilidad esperada de elegir una mujer según partido político'
plot.subtitle <- 'Cámara de Diputados: Argentina, 1983-2021\n'

# plot.caption <- ''

# axis
y_lim <- c(0, 55)
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

# p <- p + scale_x_continuous( limits = x_lim, breaks = x_bks)
p <- p + scale_y_continuous( limits = y_lim, breaks = y_bks)

# plot title
p <- p + labs( y = 'Probabilidad esperada\n')

p <- p + # ggtitle( plot.title, subtitle = plot.subtitle ) + 
  theme( plot.title = element_text(face = "bold", colour = "black", size = 17.5, lineheight = .75), plot.title.position = 'panel' ) +
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

p <- p + geom_label( aes( y = upr + 2, label = lab ), 
                     size = 4.25, label.size = NA)




## store   -----------------------------------------------
## figure

art_store[['Fig-4']] <- p

# aspect ratio
# dpi = 800, width = 12.75, height = 4




rm( plot_data,
    plot.title, plot.subtitle,
    y_lim, y_bks,
    p )



## Figure   -----------------------------------------------
## data

with( rep_data,
      data.frame( group = c('Sin cuotas', 'Ley de Cupos', 'Decreto 1246/00', 'Ley de Paridad'), 
                  quota = c( 0, 1, 0, 0), str_quota = c(0, 0, 1, 0), parity = c(0, 0, 0, 1),
                  seats = 2,
                  magnitude = median(magnitude),
                  female_gov = 0, 
                  turnout = median(turnout),
                  pj = 0, ucr = 0, jxc = 0, 
                  enp_votes = median(enp_votes),
                  enp_seats = median(enp_seats) ) ) %>%
  data.frame( ., predict.glm( art_store$mod[['Binomial_mod_3']], newdata = ., type = 'link',  se.fit = T) ) %>% as_tibble() %>%
  mutate( lwr = plogis(fit - se.fit * 1.96) * 100 ) %>%
  mutate( upr = plogis(fit + se.fit * 1.96) * 100 ) %>%
  mutate( fit = plogis(fit) * 100) %>% 
  arrange( fit) %>% 
  mutate( rank = 1:nrow(.)) %>%
  mutate( group = forcats::fct_reorder( group, rank)) %>%
  select( group, fit, lwr, upr ) %>% 
  mutate( lab = round(fit, 1) %>%
            ifelse( . %in% 0:50, paste(., 0, sep = '.'), .) %>%
            paste( ., '%', sep = '' ) ) -> plot_data

# controls were fixed at:
# seats = 2
# magnitude = 3
# turnout = 77.51028
# enp_votes = 2.855597
# enp_seats = 1.8





## Plot   -----------------------------------------------
## Specs

# titles
plot.title <- '\nProbabilidad esperada de elegir una mujer según diseño de las cuotas'
plot.subtitle <- 'Cámara de Diputados: Argentina, 1983-2021\n'

# plot.caption <- ''

# axis
y_lim <- c(0, 55)
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
               panel.grid.major.y = element_line(colour = "#D0D0D0", size = .50),
               panel.grid.minor = element_blank())
p <- p + theme(axis.ticks = element_blank())

# axis
p <- p +
  # theme(axis.title = element_blank()) + 
  theme(axis.title.x = element_blank() )  + 
  theme(axis.title.y = element_text(size = 14, colour = "#535353", face = "bold"))  + 
  theme(axis.text.x = element_text(size = 12, colour = "#535353", face = "bold"))  + 
  theme(axis.text.y = element_text(size = 12, colour = "#535353", face = "bold")) 

# p <- p + scale_x_continuous( limits = x_lim, breaks = x_bks)
p <- p + scale_y_continuous( limits = y_lim, breaks = y_bks)

# plot title
p <- p + labs( y = 'Probabilidad esperada\n')

p <- p + ggtitle( plot.title, subtitle = plot.subtitle ) + 
  theme( plot.title = element_text(face = "bold", colour = "black", size = 17.5, lineheight = .75), plot.title.position = 'panel' ) +
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

p <- p + geom_label( aes( y = upr + 2, label = lab ), 
                     size = 4.25, label.size = NA)




## store   -----------------------------------------------
## figure

art_store[['App Fig-1']] <- p

# aspect ratio
# dpi = 800, width = 12.75, height = 4


rm( plot_data,
    plot.title, plot.subtitle,
    y_lim, y_bks,
    p )



