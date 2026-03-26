## Representación de Mujeres en la Cámara de Diputados:
## Instituciones, Partidos y Actitudes en Argentina, 1983-2023

## Replication Data for: 
## Section 5 :: 'Las actitudes de los votantes'

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

rep_data <- haven::read_dta()                        ## load "Alles 2023-Replication Data 05 Indiv-level data.dta"




## Logit   -----------------------------------------------
## models

mod <- list()

# no weights
mod[['logit_mod_1']] <- glm( men_privileged ~ vot_fdt + vot_jxc + vot_fit + vot_lib + 
                               EDAD + female + 
                               med_educ + hi_educ + 
                               NSE_medBajo + NSE_medAlto + NSE_alto + 
                               unemp + retired ,
                             family = binomial( link = 'logit'),
                             data = rep_data)

mod[['logit_mod_2']] <- glm( quotas_unfair ~ vot_fdt + vot_jxc + vot_fit + vot_lib + 
                               EDAD + female + 
                               med_educ + hi_educ + 
                               NSE_medBajo + NSE_medAlto + NSE_alto + 
                               unemp + retired ,
                             family = binomial( link = 'logit'),
                             data = rep_data)

mod[['logit_mod_3']] <- glm( parity_dem ~ vot_fdt + vot_jxc + vot_fit + vot_lib + 
                               EDAD + female + 
                               med_educ + hi_educ + 
                               NSE_medBajo + NSE_medAlto + NSE_alto + 
                               unemp + retired ,
                             family = binomial( link = 'logit'),
                             data = rep_data)


# store results
art_store[['models']] <- mod


rm(mod)



## Results   -----------------------------------------------
## Table App-6, in Appendix

stargazer::stargazer( art_store[['models']]  ,
                      type = 'text', 
                      title = 'Attitudes toward Women in Politics',
                      no.space = T,
                      digits = 4, 
                      star.cutoffs = c(.05, .01, .001),
                      dep.var.labels = c( 'Men have more rights', 
                                          'Parity laws create privileges',  
                                          'Parity is more democratic'),
                      model.numbers = T, 
                      model.names = T, 
                      object.names = F,
                      omit.stat = c("f", "ser") 
                      )



## Figure   -----------------------------------------------
## function

plotFUN <- function( pred_mod, n_sims, plot_title ){
  
  
  ## Model   -----------------------------------------------
  ## prediction
  
  # model simulation
  mod_sim <- arm::sim(pred_mod, n.sims = n_sims)
  
  # frame
  with( rep_data,
        data.frame( group = c( 'Frente de\nIzquierda', 'Frente de\nTodos',
                               'Otros',
                               'Juntos por\nel Cambio', 'La Libertad\nAvanza'),
                    rank = 1:5, 
                    vot_fit = c(1, 0, 0, 0, 0),
                    vot_fdt = c(0, 1, 0, 0, 0),
                    vot_jxc = c(0, 0, 0, 1, 0),
                    vot_lib = c(0, 0, 0, 0, 1),
                    EDAD = median(EDAD, na.rm = T),
                    lo_educ = 0, med_educ = 1, hi_educ = 0,
                    NSE_medBajo = 0, NSE_medAlto = 1, NSE_alto = 0,
                    unemp = 0, retired = 0) ) %>%
    merge( data.frame( female = 0:1, gender = c('Hombres', 'Mujeres'))) %>%
    as_tibble() %>% 
    relocate( gender, .after = group) -> pred_frame
  
  
  # sim coefficients
  pred_frame %>% select( all_of((all.vars(formula(pred_mod))[-1]))) %>%
    data.frame( 1, .) -> X
  
  b <- as.matrix( mod_sim@coef)
  Xb <- t(as.matrix(X) %*% t(b))
  
  # prediction
  pred_frame %>%
    select( group, gender, rank) %>%
    data.frame( .,
                fit = apply(exp(Xb) / (1+exp(Xb)), 2, median) * 100,
                t(apply(exp(Xb) / (1+exp(Xb)), 2, quantile, probs = c(.025, .975))) * 100 ) %>%
    as_tibble() %>%
    rename( lwr = X2.5. ) %>%
    rename( upr = X97.5.) %>%
    mutate( group = forcats::fct_reorder( group, rank)) %>% 
    select( -rank ) %>% 
    mutate( lab = round(fit, 1) %>%
              ifelse( . %in% 0:100, paste(., 0, sep = '.'), .) %>%
              paste( ., '%', sep = '' ) ) -> plot_data
  
  
  rm(pred_frame, X, b, Xb)
  
  
  
  ## Figure   -----------------------------------------------
  ## data
  
  # titles
  plot_subtitle <- 'Encuesta de Satisfacción Política y Opinión Pública\nUniversidad de San Andrés\n'

  # axis
  if( plot_data$fit %>% max() > 60 ) {
    
    y_lim <- c(0, 100)
    y_bks <- seq(0, 100, length.out = 5)
    
  } else {
    
    y_lim <- c(0, 60)
    y_bks <- seq(0, 60, length.out = 4)
    
  }
  
  
  
  
  ## Figure   -----------------------------------------------
  ## script
  
  # element
  p <- ggplot( data = plot_data %>% 
                 filter(!group %in% 'Otros'),
               aes(x = group, y = fit ) )
  
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
  
  p <- p + 
    ggtitle( plot_title ) + 
    # ggtitle( plot_title, subtitle = plot_subtitle ) + 
    theme( plot.title = element_text(face = "bold", colour = "black", size = 17.5, lineheight = .75), plot.title.position = 'panel' ) +
    theme( plot.subtitle = element_text(face = "bold", colour = "#3C3C3C", size = 16, lineheight = .75))
  
  # plot caption
  # p <- p + labs( caption = plot.caption) + theme( plot.caption = element_text(size = 10, colour = "#6C6C6C", face = "bold"))
  
  # no legend
  p <- p + theme(legend.position = "none")
  
  # x-axis
  p <- p + geom_hline(yintercept = 0, linewidth = 1.2, colour="#535353")
  
  # plot content
  p <- p + geom_pointrange( aes( ymin = lwr, ymax = upr)  )
  p <- p + geom_point( size = 2.5, colour = 'black', fill = '#D0D0D0', stroke = .75, pch = 21 )
  
  p <- p + geom_label( aes( y = upr + 2, label = lab ), size = 4.25, label.size = NA)
  
  p <- p + facet_wrap( vars(gender), ncol = 2) +
    theme(strip.background = element_blank()) + 
    theme(strip.text = element_text(size = 13, colour = "black", face = "bold", vjust = .75))
  
  
  
  rm( y_lim, y_bks )
  
  return(p)
  
  
}




## Create   -----------------------------------------------
## figures

art_store[["Fig-7-1"]] <- plotFUN( art_store$models[['logit_mod_1']], 50000,
                                   '\nEn nuestro país, los hombres tienen más derechos\nque las mujeres')

art_store[["Fig-7-2"]] <- plotFUN( art_store$models[['logit_mod_2']], 50000,
                                   '\nLas leyes de paridad de género, al reservar lugares para las mujeres,\nlo que hacen es crear nuevos privilegios')

art_store[["Fig-7-3"]] <- plotFUN( art_store$models[['logit_mod_3']], 50000,
                                   '\nUn gobierno en el que las mujeres ocupan la mitad de los cargos\nes un gobierno más democrático')



