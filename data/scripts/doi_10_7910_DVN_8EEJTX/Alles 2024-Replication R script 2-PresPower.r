### Institutional Opportunism: How Electoral Fortunes Shape
### Preferences for Power-Sharing Institutions in Latin America

## Legislative Studies Quarterly
## DOI: 10.1111/lsq.12472

## Santiago Alles
## Departamento de Ciencias Sociales
## Universidad de San Andrés
## <salles@udesa.edu.ar>


## Replication Data for: 
## Section 7 :: 'The Power of the President: Defending the Realm'


## updated: 2024-07-15


## Loading
## Packages

require(dplyr)
require(lme4)

require(ggplot2)
require(ggsci)



rm(list = ls())



article_store <- list()


## Load   -----------------------------------------------
## data

# PELA Data
rep_data <- haven::read_dta()                        ## load "Alles 2024-LSQ 10.1111lsq.12472-Replication Data.dta"


# Shape data
geo <- list()

geo[['geo']] <- sf::st_read(dsn = ., layer = .)                ## load shapes: "TM_WORLD_BORDERS-0.3"
geo[['codes']] <- read.csv()                                   ## load: "TM_WORLD_BORDERS-0.3 - CountryCode.csv"



## Model    -----------------------------
## Data

# select sample
(rep_data %>% filter( !is.na(PRES_05)))$wave_code %>% unique() -> PRES_05_wav
(rep_data %>% filter( !is.na(PRES_06)))$wave_code %>% unique() -> PRES_06_wav

# data
rep_data %>% 
  filter( wave_code %in% c(PRES_05_wav, PRES_06_wav)) %>%
  mutate( incumbent_seats = incumbent_party * party_seats_pct ) %>%
  mutate( party_seats_sq = party_seats_pct ^ 2 ) -> mod_data




## Model    -----------------------------
## Estimation

## (1) linear model: country- and wave- fixed effects

lmer( pres_power ~ incumbent_party + pres_leg + pres_nonleg + leg_ideology + party_seats_pct + 
        (1 | iso_alpha_3) + 
        (1 | wave_group), 
        data = mod_data) -> article_store[['mod0_fe']]

lmer( pres_power ~ incumbent_party + pres_leg + pres_nonleg + leg_ideology + party_seats_pct + 
        incumbent_seats + party_seats_sq + 
        (1 | iso_alpha_3) + 
        (1 | wave_group), 
        data = mod_data) -> article_store[['mod1_fe']]

lmer( pres_power ~ incumbent_party + pres_leg + pres_nonleg + leg_ideology + party_seats_pct + 
        incumbent_seats + party_seats_sq +
        age + gender + leg_exp + party_age_log + pres_right + pres_left + Pres_Approval + 
        (1 | iso_alpha_3) + 
        (1 | wave_group), 
        data = mod_data) -> article_store[['mod2_fe']]

lmer( pres_power ~ incumbent_party + pres_leg + pres_nonleg + leg_ideology + party_seats_pct + 
        incumbent_seats + party_seats_sq + 
        age + gender + leg_exp + party_age_log + pres_right + pres_left + Pres_Approval + 
        dem_years + fh_average + GDP_growth_lag + inflation_lag + 
        (1 | iso_alpha_3) + 
        (1 | wave_group), 
        data = mod_data) -> article_store[['mod3_fe']]


## (2) linear model: country- and wave- fixed effects (weighted data)

lmer( pres_power ~ incumbent_party + pres_leg + pres_nonleg + leg_ideology + party_seats_pct + 
        (1 | iso_alpha_3) + 
        (1 | wave_group), 
        weights = party_wt, 
        data = mod_data) -> article_store[['mod0_fe_wt']]

lmer( pres_power ~ incumbent_party + pres_leg + pres_nonleg + leg_ideology + party_seats_pct + 
        incumbent_seats + party_seats_sq + 
        (1 | iso_alpha_3) + 
        (1 | wave_group), 
        weights = party_wt, 
        data = mod_data) -> article_store[['mod1_fe_wt']]

lmer( pres_power ~ incumbent_party + pres_leg + pres_nonleg + leg_ideology + party_seats_pct + 
        incumbent_seats + party_seats_sq + 
        age + gender + leg_exp + party_age_log + pres_right + pres_left + Pres_Approval + 
        (1 | iso_alpha_3) + 
        (1 | wave_group), 
        weights = party_wt, 
        data = mod_data) -> article_store[['mod2_fe_wt']]

lmer( pres_power ~ incumbent_party + pres_leg + pres_nonleg + leg_ideology + party_seats_pct + 
        incumbent_seats + party_seats_sq +
        age + gender + leg_exp + party_age_log + pres_right + pres_left + Pres_Approval + 
        dem_years + fh_average + GDP_growth_lag + inflation_lag + 
        (1 | iso_alpha_3) + 
        (1 | wave_group), 
        weights = party_wt, 
        data = mod_data) -> article_store[['mod3_fe_wt']]




## Model    -----------------------------
## Results

## Note: Table 2 in the article is a short version of Table III-2, in the online appendix.

tab_title <- 'Preferences over Presidential Power: Latin American Legislators, 2002-2012 (based on PELA data).'


# (1) linear models, country- and wave- fixed effects
stargazer::stargazer( article_store[['mod0_fe']], 
                      article_store[['mod1_fe']], 
                      article_store[['mod2_fe']], 
                      article_store[['mod3_fe']], 
                      title = tab_title %>% paste( 'Table III-2 -', ., 'Mixed-effects linear models.'),
                      type = "text",
                      dep.var.labels = '',
                      no.space = T,
                      digits = 4, star.cutoffs = c( .05,.01,.001),
                      omit.stat = c("f", "ser") )


# (2) linear models, country- and wave- fixed effects, weighted data
stargazer::stargazer( article_store[['mod0_fe_wt']], 
                      article_store[['mod1_fe_wt']], 
                      article_store[['mod2_fe_wt']], 
                      article_store[['mod3_fe_wt']], 
                      title = tab_title %>% paste('Table III-4 -', ., 'Mixed-effects linear models, with weighted data.'),
                      type = "text",
                      dep.var.labels = '',
                      no.space = T,
                      digits = 4, star.cutoffs = c( .05,.01,.001),
                      omit.stat = c("f", "ser") )


rm(tab_title)



## Model coef    -----------------------------
## posterior distributions

# prediction model
pred_mod <- article_store[['mod2_fe']]

# posterior distributions
mod_sim <- arm::sim( pred_mod, n.sims = 50000)




## Create    -----------------------------
## Maps

# figure title
plot_title <- 'Preferences over Power Concentration'
plot_subtitle <- 'Based on PELA data, 2002-2012 (Universidad de Salamanca)'

# country-average 
data.frame( (mod_sim@ranef$iso_alpha_3 %>% dimnames)[[2]],
            mod_sim@ranef$iso_alpha_3 %>%
              as.data.frame() %>%
              apply(., 2, mean),
            mod_sim@ranef$iso_alpha_3 %>% 
              as.data.frame() %>% 
              apply(., 2,  quantile, probs = c(.5, .05, .95) ) %>%
              t() ) %>% 
  as_tibble() -> country_ranef

colnames(country_ranef) <- c('iso_alpha_3', 'mean', 'median', 'ci_lo', 'ci_up')

geo$codes %>% 
  left_join( country_ranef, by = join_by(iso_alpha_3) ) %>% 
  mutate( mod_intercept = (mod_sim@fixef %>% as.data.frame() %>%
                             apply(., 2, mean))[1] ) %>% 
  mutate( expect_ave = mean + mod_intercept) %>%
  mutate( group = 'PR Rules') %>% 
  dplyr::select(country_label, iso_alpha_3, iso_alpha_2, group, mean, ci_lo, ci_up, 
                mod_intercept, expect_ave) %>% 
  arrange(mean) %>% filter(!is.na(mean)) -> country_ranef

# combine data & shapes
geo[['geo']] %>% left_join( country_ranef, by = c('ISO3' = 'iso_alpha_3')) -> map_data

# map specs
with( country_ranef %>% filter(!is.na(mean)),
      c( min(mean) - (max(mean) - min(mean)) * .025, 
         max(mean) + (max(mean) - min(mean)) * .025) 
      ) -> fill_lim

fill_brk <- seq(fill_lim[1], fill_lim[2], length.out = 5)
fill_col <- 'brown'

legend_title <- 'Favorable to:\n'
legend_lab <- c('Stronger\nCongress', 'Stronger\nPresident')
legend_lab <- rep('', length(fill_brk) - 2) %>%  c( legend_lab[1], ., legend_lab[2] )


# ggplot element
map <- ggplot( map_data %>% filter(!is.na(mean)), aes(geometry = geometry, fill = mean) )

# map region
map <- map + theme_bw()
map <- map + theme( panel.border = element_rect(colour="white"))

# grid format
map <- map + theme( panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
map <- map + theme( axis.ticks = element_blank())

# title
map <- map + ggtitle( plot_title) + theme( plot.title = element_text(face="bold", colour="black", size=24))

# axis text & marks
map <- map + theme(axis.text = element_blank() ) + theme(axis.title = element_blank() )

# plot legend
map <- map + theme( legend.background = element_rect(fill="#FFFFFF"),
                    legend.title = element_text(size = 17.5, colour = "black", face = "bold" , lineheight = .75),
                    legend.text = element_text(size = 15, colour = "#3C3C3C", lineheight = .8, vjust = .5),
                    legend.key = element_rect(fill = "#FFFFFF", colour = NA),
                    legend.key.width = unit(0.9, "cm"), legend.key.height = unit(1.25, "cm") ,
                    legend.spacing.x = unit(0.2, "cm"),
                    legend.position = c(0.150, 0.275))

# scale gradient
map <- map + 
  scale_fill_material( fill_col, breaks = fill_brk, limits = fill_lim, labels = legend_lab ) +
  labs(fill = legend_title)

# plot content
map <- map + geom_sf( data = map_data ,
                      aes(geometry = geometry), 
                      fill = 'white', color = "black" )
map <- map + geom_sf( color = "black", na.rm = T )

map <- map + coord_sf(xlim = c(-120, -29), ylim = c(-54, 30))

# store map
article_store[['Figure 4']] <- map




rm( plot_title, plot_subtitle,
    fill_lim, fill_brk, fill_col,
    legend_title, legend_lab )

rm( map_data, country_ranef,
    map )




## Create    -----------------------------
## Figure 5

# Vars to predict
rbind( data.frame( party_seats_pct = c( seq(2.50, 60, .25), seq( 10, 60, 5)) %>% 
                     sort() %>% unique(), 
                   incumbent_party = 1) %>% 
         filter(party_seats_pct > 12.5 ),                                                   # filter: incumbent party > 12.5%
       data.frame( party_seats_pct = c( seq(2.50, 60, .25), seq( 10, 60, 5)) %>% 
                     sort() %>% unique(), 
                   incumbent_party = 0) %>% 
         filter(party_seats_pct < 47.5 )) %>%                                                # filter: opposition < 46%
  as_tibble() -> pred

# Control vars
omit_vars <- c('iso_alpha_3', 'wave_group')

x.control <- all.vars(formula(pred_mod))[-1]
x.control <- x.control[!x.control %in% omit_vars]

mod_data %>% 
  select( x.control[!x.control %in% c(omit_vars, colnames(pred))] %>% all_of()) %>%
  apply( ., 2, mean, na.rm = T) %>% t() %>% 
  data.frame( pred, .) %>% 
  mutate( incumbent_seats = incumbent_party * party_seats_pct ) %>%
  mutate( party_seats_sq = party_seats_pct ^ 2 ) %>% 
  select( all_of(all.vars(formula(pred_mod))[all.vars(formula(pred_mod)) %in% x.control]) ) %>%
  data.frame( 1, .) %>%
  as_tibble() -> X

# sim model coef
b <- as.matrix( mod_sim@fixef)
Xb <- na.omit(t(as.matrix(X) %*% t(b)))

data.frame( pred, 
            apply(Xb, 2, mean),
            apply(Xb, 2, quantile, probs = c(.025,.05,.95,.975)) %>% t()) %>%
  as_tibble() -> plot_data

colnames(plot_data) <- c(colnames(pred), "mean", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")

rm( X, Xb, b,
    x.control, omit_vars, 
    pred)

# plot data
plot_data %>%
  mutate( label = ifelse( incumbent_party == 1, "Incumbent Party", "Opposition Party" ) 
  ) -> plot_data

mod_data %>%
  filter(!is.na(incumbent_party)) %>%
  mutate( label = ifelse( incumbent_party == 1, "Incumbent Party", "Opposition Party" ) 
  ) -> rug_data

mod_data %>% 
  filter(!is.na(incumbent_party)) %>%
  group_by(incumbent_party) %>% 
  summarise( y_pos = mean( pres_power, na.rm = T) ) %>%
  mutate( lab = round(y_pos, 1)) %>% 
  mutate( lab = ifelse( lab %in% seq(1, 10 ), paste(lab, '.0', sep = ''), lab)) %>%
  mutate( label = ifelse( incumbent_party == 1, "Incumbent Party", "Opposition Party" )) %>%
  mutate( x_pos = ifelse( incumbent_party == 1, 7.5, 52.5 )) %>%
  select( label, incumbent_party, lab, y_pos, x_pos) -> mean_data

# plot specs
legend_lab <- c('Leg', 'Ex')

y_brk <- 1:5

y_lab <- y_brk[2:(length(y_brk)-1)] 
y_lab <- y_lab%>% c(legend_lab[1], ., legend_lab[2])

x_brk <- seq(0, 60, 15)
x_lab <- paste(x_brk, "%", sep="")

y_lim <- c(.75, 5)
x_lim <- c(0, 60)

# ggplot element
p <- ggplot( plot_data  %>% filter(party_seats_pct <= 57.5 ), aes(x = party_seats_pct, y = mean ))

# Chart region
p <- p + theme_bw()
p <- p + theme(panel.background = element_rect(fill = "#FFFFFF")) +
  theme(plot.background = element_rect(fill = "#FFFFFF")) +
  theme(panel.border = element_rect(colour = "#FFFFFF"))

# grid
p <- p + theme( panel.grid.major.x = element_line(colour="#D0D0D0", linewidth = .25),
                panel.grid.major.y = element_line(colour="#D0D0D0", linewidth = .50),
                panel.grid.minor = element_blank())
p <- p + theme( axis.ticks = element_blank())

# legend
p <- p + theme( legend.background = element_rect(fill = "#FFFFFF"),
                legend.text = element_text(size = 11, colour = "#3C3C3C", face = "bold"),
                legend.title = element_blank(),
                legend.key = element_rect(fill = "#FFFFFF", colour = NA),
                legend.position = c(0.175, 0.775))

# axis
p <- p + labs(y = "Expected Preference\n(95% and 90% c.i.)", x = "Percentage of House Seats")
p <- p + 
  theme(axis.text = element_text(size = 11, colour = "#535353", face = "bold")) + 
  theme(axis.title = element_text(size = 11, colour = 'black', face = "bold", vjust = .5))

p <- p + scale_x_continuous(limits = x_lim, breaks = x_brk, labels = x_lab )
p <- p + scale_y_continuous(limits = y_lim, breaks = y_brk, labels = y_lab )

# x axis
p <- p + geom_hline(yintercept = 1, linewidth = 1.2, colour = "#535353")

# group means
p <- p + geom_linerange( data = mean_data,
                         aes(y = y_pos , xmin = min(x_lim) + 1.5, xmax = max(x_lim) - 2.5), 
                         inherit.aes = F,
                         linewidth = .4, colour="red", linetype = "longdash")

p <- p + geom_label( data = mean_data,
                     aes(x = x_pos, y = y_pos, label = lab ),
                     # hjust = 0, 
                     colour = 'white', fill = 'red', size = 4.5 )

# plot content
p <- p + geom_ribbon( aes(ymax = ci.up.90, ymin = ci.lo.90), alpha = .15)
p <- p + geom_ribbon( aes(ymax = ci.up.95, ymin = ci.lo.95), alpha = .10)

p <- p + geom_line( linewidth = 1.25)

p <- p + facet_wrap( ~ label) +
  theme(strip.background = element_blank()) + 
  theme(strip.text = element_text(size = 14, colour = "black", face = "bold", vjust = .75))

# plot rug
p <- p + geom_rug( data = rug_data, inherit.aes = F, 
                   aes(x = party_seats_pct, y = 0.85),
                   alpha = .35, colour = "darkgreen", 
                   position = position_jitter(width = 2.5), 
                   sides = "b")  

# store plot
article_store[['Figure 5']] <- p


rm( plot_data, 
    mean_data, rug_data,
    legend_lab,
    x_brk, x_lab, x_lim,
    y_brk, y_lab, y_lim,
    p )



## Create    ----------------------------------
## Figure 6

# plot data
desc_fig_data <- mod_data %>% filter( wave_code %in% c(PRES_05_wav, PRES_06_wav))

# subset of interest
PRES_countries <- c("CHL", "BRA", "MEX")
PRES_parties <- c( "CHL:RN", 'CHL:PPD', "BRA:PSDB","BRA:PT", "MEX:PAN", "MEX:PRD", "MEX:PRI" )

# Floor Average
(desc_fig_data %>% 
    select(iso_alpha_3, interview_year, pres_power, party_wt) %>%
    na.omit() %>% 
    data.table::as.data.table())[, lapply( .SD, weighted.mean, w = party_wt), by = list(iso_alpha_3, interview_year) ] %>%
  as.data.frame() %>% 
  select(iso_alpha_3, interview_year, pres_power) %>% 
  rename( PRES_mean = pres_power ) %>%
  arrange(iso_alpha_3, interview_year) %>%
  as_tibble() -> PRES_Cstore

geo$codes %>% 
  filter(iso_alpha_3 %in% PRES_countries) %>% 
  select(country_label, iso_alpha_3, iso_alpha_2) %>%
  left_join( PRES_Cstore %>%
               filter(iso_alpha_3 %in% PRES_countries), 
             by = c('iso_alpha_3' = 'iso_alpha_3')) -> plot_aveC

# Party Averages
desc_fig_data %>%
  group_by(iso_alpha_3, party_label, interview_year) %>%
  summarize(PRES_mean = mean(pres_power, na.rm = T),
            PRES_sd = sd(pres_power, na.rm = T)) %>% 
  arrange(iso_alpha_3, interview_year, party_label) -> PRES_Pstore

geo$codes %>% 
  filter(iso_alpha_3 %in% PRES_countries) %>% 
  select(country_label, iso_alpha_3, iso_alpha_2) %>% 
  left_join( PRES_Pstore %>% 
               filter(iso_alpha_3 %in% PRES_countries), 
             by = 'iso_alpha_3' ) %>% 
  left_join( desc_fig_data %>% 
               filter( iso_alpha_3 %in% PRES_countries) %>%
               count(party_label, interview_year),
             by = join_by(party_label, interview_year) ) %>% 
  rename( obs = n ) -> plot_aveP

c( paste(plot_aveP$iso_alpha_3, 'OTHER', sep = ':'), 
   paste(plot_aveP$iso_alpha_3, 'IND',   sep = ':') ) %>% 
  unique() -> other_parties

# Plot cutlines
data.frame( country_label = "Chile", 
            x = 2009.25, 
            y = 1.5, 
            yend = 3.5) %>% 
  filter(country_label %in% plot_aveC$country_label) %>%
  as_tibble() -> plot_cutline

# Plot text (1)
plot_aveP %>%
  left_join( plot_aveP %>%
               group_by(iso_alpha_3) %>%
               summarize( min_year = min(interview_year),
                          max_year = max(interview_year) ),
             by = join_by(iso_alpha_3) ) %>%
  rename( label = party_label ) %>% 
  filter(label %in% PRES_parties) %>%
  mutate( label = gsub( paste(PRES_countries %>% paste(':', sep = '') , collapse = '|'), '', label ) ) %>% 
  filter( interview_year == min_year ) %>%
  mutate( xpos = interview_year + -.3 ) %>%
  mutate( ypos = PRES_mean ) %>%
  mutate( hjust = 1 ) -> plot_textP 





# Plot text (2)
plot_aveC %>%
  group_by(country_label, iso_alpha_3, iso_alpha_2) %>%
  summarize(interview_year = max(interview_year )) %>%
  left_join( plot_aveC,
             by = join_by(country_label, iso_alpha_3, iso_alpha_2, interview_year) ) %>%
  mutate( xpos = interview_year + .2) %>%
  mutate( ypos = PRES_mean) %>%
  mutate( hjust = 0 ) -> plot_textF

# plot specs
legend_lab <- c('Leg', 'Ex')

y_brk <- 1:5

y_lab <- y_brk[2:(length(y_brk)-1)] 
y_lab <- y_lab%>% c(legend_lab[1], ., legend_lab[2])

y_lim <- c(1, 5)

# ggplot element
p <- ggplot( data = plot_aveP %>% 
               filter(party_label %in% PRES_parties) , 
             aes( x = interview_year, y = PRES_mean ))

# chart region
p <- p + theme_bw()
p <- p + theme(panel.background = element_rect(fill="#FFFFFF")) +
  theme(plot.background = element_rect(fill="#FFFFFF")) +
  theme(panel.border = element_rect(colour="#FFFFFF"))

# grid
p <- p + theme(panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(colour = "#D0D0D0", linewidth = .5),
               panel.grid.minor = element_blank())
p <- p + theme(axis.ticks = element_blank())

# axis
p <- p + labs(y = "Average Preference", x=  "")
p <- p + theme(axis.text = element_text(size = 11, colour = "#535353", face = "bold")) + 
  theme(axis.title = element_text(size = 11, colour = 'black', face = "bold", vjust = .5))

p <- p + geom_hline(yintercept = 1, linewidth = 1.2, colour = "#535353")
p <- p + theme(axis.text = element_text(colour = "black"))

p <- p + scale_y_continuous(limits = y_lim, breaks = y_brk, labels = y_lab )
p <- p + scale_x_continuous(limits = c(2001, 2011), breaks = seq(2002, 2010, 2))

# No Legend
p <- p + theme(legend.position = "none")

# colours
p <- p + scale_colour_manual(values=c('red', 'black', rep(c('blue','red', 'black'), 2)))

# panels
p <- p + 
  facet_grid( . ~ country_label )  +
  theme( strip.background = element_blank()) + 
  theme( strip.text = element_text(size = 12, colour = "black", face = "bold", vjust = .75))

# plot content
p <- p + geom_segment( data = plot_cutline,
                       aes( group = country_label,
                            x = x, xend = x, y = y, yend = yend), 
                       colour="black", linetype = "dotted")
p <- p + geom_text( data = plot_cutline,
                    aes( x = x, y = y - .2, label = "Party\nSwitch"),
                    size = 2.5, colour="black", lineheight = .65 )

p <- p + geom_line( data = plot_aveC, aes(  x = interview_year, y = PRES_mean  ),
                    colour = "black", linetype = "longdash")
p <- p + geom_text( data = plot_textF,
                    aes( x = xpos, y = ypos, hjust = hjust),
                    label = 'Floor\nmean',
                    size = 2.85, colour = "black", lineheight = .65)

p <- p + geom_line( aes( group = party_label, colour = party_label ), linewidth = 1.5)
p <- p + geom_point( aes( group = party_label ),
                     size = 2.25, colour="black", fill = 'white', pch = 21)

p <- p + geom_text( data = plot_textP,
                    aes( x = xpos - .1, y = ypos, hjust = hjust, label = label),
                    size = 3.25, colour = "#3C3C3C", fontface = "bold")

# store plot
article_store[['Figure 6']] <- p



rm( desc_fig_data,
    PRES_Pstore, PRES_Cstore, PRES_countries, PRES_parties, other_parties, 
    plot_aveP, plot_aveC, plot_cutline,
    plot_textP, plot_textF, 
    legend_lab, y_brk, y_lab, y_lim,
    p )





















## Create    -----------------------------
## Figure III-1 (lower panel)

# figure title
plot_title <- 'Preferences over Power Concentration'

# frame
data.frame( (mod_sim@ranef$iso_alpha_3 %>% dimnames)[[2]],
            mod_sim@ranef$iso_alpha_3 %>%
              as.data.frame() %>%
              apply(., 2, mean),
            mod_sim@ranef$iso_alpha_3 %>% 
              as.data.frame() %>% 
              apply(., 2,  quantile, probs = c(.5, .05, .95) ) %>%
              t() ) %>% 
  as_tibble() -> plot_data

colnames(plot_data) <- c('iso_alpha_3', 'mean', 'median', 'ci_lo', 'ci_up')

plot_data %>% 
  arrange(mean) %>%
  mutate( rank = seq(1, nrow(.) ) ) -> plot_data

# plot specs
legend_lab <- c('Stronger\nCongress', 'Stronger\nPresident')

x_lim <- c( -1.25, 1.25)

x_brk <- seq( -1, 1, .25)
x_brk <- x_brk[!x_brk %in% 0]

x_lab <- c(legend_lab[1], x_brk[2:(length(x_brk)-1)], legend_lab[2] )
x_lab <- ifelse( x_lab %in% -10:10, paste( x_lab, '0', sep = '.' ), x_lab)

y_lim <- c(0.5, nrow(plot_data) + .5)
y_brk <- seq(1, nrow(plot_data))
y_lab <- plot_data$iso_alpha_3

# ggplot element
p <- ggplot(plot_data, aes(y = rank, x = mean, xmax = ci_up, xmin = ci_lo))

# chart region
p <- p + theme_bw()
p <- p + theme( panel.background = element_rect(fill = "#FFFFFF")) +
  theme( plot.background = element_rect(fill = "#FFFFFF")) +
  theme( panel.border = element_rect(colour = "#FFFFFF"))

# grid
p <- p + theme( panel.grid.major.x = element_line(colour = "#D0D0D0", linewidth = .25),
                panel.grid.major.y = element_line(colour = "#D0D0D0", linewidth = .50),
                panel.grid.minor = element_blank())
p <- p + theme( axis.ticks = element_blank())

# title
p <- p + ggtitle(plot_title) + 
  theme( plot.title = element_text(face = "bold", colour = "black", size = 18))

# axis
p <- p + labs(x = "\nCountry-level Intercept, and 90% CI", y = "")

p <- p + 
  theme( axis.text.x = element_text(size = 12, colour = "#535353", face = "bold")) + 
  theme( axis.text.y = element_text(size = 10, colour = "#535353", face = "bold")) + 
  theme( axis.title = element_text(size = 13, colour = "black", face = "bold", vjust = .5))

p <- p + scale_x_continuous(limits = x_lim, breaks = x_brk, labels = x_lab )
p <- p + scale_y_continuous(limits = y_lim, breaks = y_brk, labels = y_lab )

p <- p + geom_vline(xintercept = 0, linewidth = .65, linetype = 2, colour = "#535353")

# content
p <- p + geom_linerange()
p <- p + geom_point(size = 2.25, colour = "black", fill = '#D0D0D0', pch = 21)

# store plot
article_store[['Figure III-1-lower']] <- p



rm( plot_data, 
    plot_title, legend_lab, 
    y_lim, y_brk, y_lab,
    x_lim, x_brk, x_lab,
    p )


rm( mod_sim, pred_mod)



## Data    -----------------------------
## Summary

# Table IV-2: observations by country
geo$codes %>% 
  select(country_label, iso_alpha_3, iso_alpha_2) %>%
  left_join( mod_data %>%
               filter( wave_code %in% c(PRES_05_wav, PRES_06_wav)) %>%
               filter(!is.na(incumbent_party)) %>%
               count(iso_alpha_3) %>%
               mutate( PRES_Power_pct = n / sum(n) * 100) %>%
               rename( PRES_Power = n ), 
             by = join_by(iso_alpha_3)) %>% 
  filter(!is.na(PRES_Power)) %>%
  knitr::kable( digits = 2, row.names = F ) -> article_store[['Table IV-2']]

# Table IV-4: descriptive statistics
mod_data %>%
  filter(!is.na(incumbent_party)) %>%
  dplyr::select( pres_power, 
                 party_seats_pct, incumbent_party, party_age, party_age_log,
                 pres_leg, pres_nonleg, dem_years, fh_average, 
                 age, gender, leg_exp, leg_ideology,
                 pres_right, pres_left, Pres_Approval,
                 GDP_growth_lag, inflation_lag) %>% 
  as.data.frame() %>%
  stargazer::stargazer( ., 
                        title = "Table IV-4. Descriptive statistics: Presidential Power models", 
                        type = "text",
                        no.space = T,
                        digits = 3,
                        summary.stat = c("n", "mean", "median", "min", "max", 'sd'))



rm( PRES_05_wav, PRES_06_wav, mod_data )

