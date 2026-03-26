### Who Supports Gender Equality?
### Age, Education, and Sexism in the Shadow of the Radical Right in Argentina

## European Journal of Politics and Gender
## DOI: 10.1332/25151088Y2025D000000104

## Santiago Alles
## Departamento de Ciencias Sociales
## Universidad de San Andrés
## <salles@udesa.edu.ar>


## Model predictions 
## Figure 1 :: Right-panel


## updated: 2025-08-25




## Model   -----------------------------------------------
## prediction

# model simulation
mod_sim <- arm::sim(pred_mod, n.sims = n_sims)

# frame
data.frame( 
  
  # Gender
  gender = c('Men', 'Women'), female = 0:1) %>% 
  
  # Education
  merge( data.frame( education = c('Some High-school\nor less', 'High-school\ndiploma',
                                   'Some higher ed.\nNon-univ. degree', 'College degree\nor more') ,
                     lo_educ = c(1, 0, 0, 0),
                     medLo_educ = c(0, 1, 0, 0),
                     medUp_educ = c(0, 0, 1, 0),
                     hi_educ = c(0, 0, 0, 1) ) %>% 
           mutate( rank = 1:nrow(.))) %>% 
  
  # interactions: gender x education
  mutate( fem_medLo = female * medLo_educ) %>%
  mutate( fem_medUp = female * medUp_educ) %>%
  mutate( fem_hi = female * hi_educ) %>%
  
  # controls
  mutate( age = median(mod_data$age) ) %>% 
  mutate( age_sq = age^2 ) %>% 
  
  data.frame( pub_employee = 0, unemp = 0, retired = 0, inactive = 0) %>% 
  
  as_tibble() -> pred_frame

# fixed-effect variables
fe_vars <- c('sample', 'region')

# matrix
pred_frame %>% 
  select(all_of(all.vars(formula(pred_mod))[-1][!all.vars(formula(pred_mod))[-1] %in% fe_vars])) %>% 
  tibble( 1, .) -> X

# coefficients
b <- as.matrix( mod_sim@fixef)
Xb <- na.omit(t(as.matrix(X) %*% t(b)))

# prediction
pred_frame %>% 
  select(gender, education, rank) %>% 
  data.frame( apply(Xb, 2, mean),
              apply(Xb, 2, quantile, probs = c(.025,.05,.95,.975)) %>% t()) %>%
  mutate( education = forcats::fct_reorder( education, rank)) %>%
  as_tibble() -> plot_data

colnames(plot_data) <- c('gender', 'education', 'rank', "mean", "ci_lo_95", "ci_lo_90", "ci_up_90", "ci_up_95")

plot_data <- plot_data %>% arrange(gender, education) %>% select( -rank)


rm( X, Xb, b,
    fe_vars, 
    pred_frame )



## Plot   -----------------------------------------------
## data

# axis
y_lim <- c(-.85, .85)
y_bks <- seq(-.8, .8, length.out = 5)
y_bks <- y_bks[!y_bks %in% 0]




## Plot   -----------------------------------------------
## script

# element
p <- ggplot( data = plot_data,
             aes( x = education, y = mean, group = gender ) )

# chart region
p <- p + theme_bw()
p <- p + theme( panel.border = element_rect(colour = "white"))

# grid
p <- p + theme( panel.grid.major.x = element_line(colour = "#D0D0D0", linewidth = .25),
                panel.grid.major.y = element_line(colour = "#D0D0D0", linewidth = .50),
                panel.grid.minor = element_blank())
p <- p + theme( axis.ticks = element_blank())

# axis
p <- p +
  theme(axis.title = element_text(size = 15, colour = "black", face = "bold")) + 
  theme( axis.text.x = element_text(size = 12, colour = "#535353", face = "bold"))  + 
  theme( axis.text.y = element_text(size = 14, colour = "#535353", face = "bold"))

p <- p + scale_y_continuous( limits = y_lim, breaks = y_bks, position = "right")

p <- p + labs( y = 'Modern Sexism Index\n', x = "\nRespondent's Education")

# colours
p <- p + scale_fill_manual( values = c('white', 'black'))

# legend
p <- p + theme( legend.background = element_rect(fill="#FFFFFF"),
                legend.text = element_text(size = 12, colour = "#3C3C3C", face = "bold" ),
                legend.title = element_blank(),
                legend.key = element_rect(fill = "#FFFFFF", colour = NA),
                legend.position = "inside",
                legend.position.inside = c(0.15, 0.175))

# x-axis
p <- p + geom_hline(yintercept = 0, linewidth = .7, colour="#535353", linetype = 'dashed')

# plot content
p <- p + geom_linerange( aes(ymax = ci_up_95, ymin = ci_lo_95, group = gender), position = position_dodge(width = 0.4))
p <- p + geom_point( aes(fill = gender), size = 3.0, stroke = .75, pch = 21, position = position_dodge(width = 0.4))

# store
plot_store[['Educ_plot']] <- p
plot_store[['Educ_p_data']] <- plot_data





rm( y_lim, y_bks, 
    plot_data, mod_sim, 
    p)



