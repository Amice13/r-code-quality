### The Burden of Election Logistics
### Election Ballots and the Territorial Influence of Party Machines in Colombia

### The Journal of Politics, forthcoming.

### Santiago Alles  <santiago.alles@voxel-consulting.com>
### Monica Pachon   <mopachon@uniandes.edu.co>
### Manuela Munoz   <manuela.munozf@tamu.edu>

### updated: 04/20/2020


### Loading
### Packages

require(tidyverse)
require(ggplot2)



### Figure 5
### Part (i)

## Plot
## Data

## Votes
plot_dat <- wVote_dpt_store %>% 
  filter( year >= year_min & year <= year_max) %>%         ## keep 1970-2002
  filter( !department_code %in% dept_filter )              ## exclude Bogota, DC 

plot_dat %>% filter(!is.na(D_wV)) -> plot_dat

## Axis
y_lim <- with(plot_dat, ceiling(max(D_wV)))


## Plot
## Script

# ggplot element
p <- ggplot( data = plot_dat, aes(x = as.factor(year), y = D_wV ))

# region
p <- p + theme_bw()
p <- p + theme(panel.border=element_rect(colour="white"))


# grid
p <- p + theme(panel.grid.major.x=element_line(colour="#D0D0D0",size=.25),
               panel.grid.major.y=element_line(colour="#D0D0D0",size=.5),
               panel.grid.minor=element_blank())
p <- p + theme(axis.ticks=element_blank())

# axis & marks
p <- p + labs(y = "Wasted Votes (%), by Department\n", x="")

p <- p + theme(axis.text = element_text(size = 18, colour = "#535353", face="bold")) + 
  theme(axis.title = element_text(size = 20, colour = 'black', face="bold", vjust=.5))

p <- p + scale_y_continuous(limits = c(0, y_lim), breaks = seq(0, 50, length = 6) )

p <- p + geom_hline(yintercept = 0, size = 1.2, colour="#535353")

# content
p <- p + geom_jitter( size = 2.5, width = .125, height = 0, alpha = 0.35, fill = "red", colour = "red")
p <- p + geom_boxplot( width = .4, outlier.shape = NA, colour = 'black', alpha = 0.35)


# save
ggsave(paste(plot_dir, 'Figure 5-A.png', sep = "/"), p, dpi = 800, width = 15, height = 7.5)



rm( plot_dat, p, y_lim )



### Figure 5
### Part (ii)

## Plot
## Data

## Votes
mod_data <- wVote_mjP_store %>% 
  filter( year >= year_min & year <= year_max ) %>%         ## keep 1970-2002
  filter( !department_code %in% dept_filter ) %>%           ## exclude Bogota, DC 
  filter(  party_code %in% c(1,2) ) %>% 
  drop_na(waste_pct, year, party_seats, magnitude)

with(mod_data, ifelse(party_code == 1, 1, 0)) -> mod_data$liberal
with(mod_data, ifelse(party_code == 1, 'Liberal Party', 'Conservative Party')) -> mod_data$party_lab

## Axis
y_lim <- with(mod_data, ceiling(max(waste_pct)))



## Loess
## Model

mod <- as.formula(waste_pct ~ year + party_seats + magnitude)

mod_1 <- loess( mod,
                family = "gaussian", method = "loess",
                data = mod_data %>% filter(liberal == 1) )

mod_2 <- loess( mod,
                family = "gaussian", method = "loess",
                data = mod_data %>% filter(liberal == 0) )


## Model
## Prediction

## Frame
subset( mod_data, select = c(all.vars(mod), 'department_code', 'liberal', 'party_lab') ) %>% 
  tbl_df() -> pred_frame


## Prediction
rbind( data.frame( pred_frame %>% filter(liberal == 1), 
                   pred = predict(mod_1, pred_frame %>% filter(liberal == 1)) ),
       data.frame( pred_frame %>% filter(liberal == 0), 
                   pred = predict(mod_2, pred_frame %>% filter(liberal == 0)) )) %>% 
  arrange(year, liberal, department_code) %>% 
  tbl_df() -> pred_frame


merge( with(pred_frame, data.frame( year = sort(unique(year)), brk = seq(1, length(unique(year))) ) ),
       pred_frame, all.y = T) %>% 
  tbl_df() -> pred_frame

pred_frame %>% 
  drop_na(waste_pct, year, party_seats, magnitude, pred) %>% 
  tbl_df() -> pred_frame


## Loess
## Plot

# ggplot element
p <- ggplot( data = pred_frame, aes(x = brk, y = pred ))


# region
p <- p + theme_bw()
p <- p + theme(panel.border=element_rect(colour="white"))

# grid
p <- p + theme(panel.grid.major.x=element_line(colour="#D0D0D0",size=.25),
               panel.grid.major.y=element_line(colour="#D0D0D0",size=.5),
               panel.grid.minor=element_blank())
p <- p + theme(axis.ticks=element_blank())

# axis & marks
p <- p + labs(y = "Votes of Sub-Party Lists (%),\nby Department\n", x="")

p <- p + theme(axis.text = element_text(size = 18, colour = "#535353", face="bold")) + 
  theme(axis.title = element_text(size = 20, colour = 'black', face="bold", vjust=.5))

p <- p + scale_y_continuous( limits = c(0, 80), breaks = seq(0, 100, length = 6) )
p <- p + scale_x_continuous( breaks = unique(pred_frame$brk), labels = sort(unique(pred_frame$year)) )

p <- p + geom_hline(yintercept = 0, size = 1.2, colour="#535353")

# content
p <- p + geom_jitter( data = pred_frame, aes(x = brk, y = waste_pct ),
                      size = 2.5, width = .25, height = 0, 
                      alpha = 0.35, fill = "red", colour = "red")

p <- p + geom_smooth( method = 'loess', se = F, level = 0.975, na.rm = T)

p <- p + facet_wrap( ~ party_lab) +
  theme(strip.background = element_blank()) + 
  theme(strip.text = element_text(size = 24, colour = "black", face = "bold", vjust = .75))


# save
ggsave(paste(plot_dir, 'Figure 5-B.png', sep="/"), p, dpi = 800, width = 15, height = 7.5)




rm( mod_data, mod, mod_1, mod_2,
    pred_frame, p,
    y_lim )




