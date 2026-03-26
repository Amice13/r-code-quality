### The Burden of Election Logistics
### Election Ballots and the Territorial Influence of Party Machines in Colombia

### The Journal of Politics, forthcoming.

### Santiago Alles  <santiago.alles@voxel-consulting.com>
### Monica Pachon   <mopachon@uniandes.edu.co>
### Manuela Munoz   <manuela.munozf@tamu.edu>

### updated: 04/20/2020


### Loading
### Packages

require(ggplot2)



### Plot
### Data

G_idx_store %>% 
  filter( year >= year_min & year <= year_max ) %>%                        ## keep 1970-2002
  filter( !department_code %in% dept_filter ) %>%                          ## exclude Bogota, DC
  dplyr::select( department_name, fips_code, iso_code, department_code,
                 year, y_total_adj_pc, y_total_adj_gini) -> plot_dat

unique(plot_dat) -> plot_dat

with(plot_dat, y_total_adj_pc * 1000 ) -> plot_dat$local_rev_pc
with(plot_dat, y_total_adj_gini ) -> plot_dat$local_rev_gini


### Figure (1)
### Revenues per Capita

## Axis & Titles
plot_title <- 'Revenues per capita\n'

y_brk <- seq(0, 10, length.out = 5) * 100
x_brk <- seq(1986, 2002, length.out = 5)


## Plot
## Script

# ggplot element
p <- ggplot( data = plot_dat %>% filter(!is.na(local_rev_pc) & local_rev_pc <= max(y_brk)), 
             aes(x = year, y = local_rev_pc ))

# region
p <- p + theme_bw()
p <- p + theme(panel.border=element_rect(colour="white"))

# grid
p <- p + theme(panel.grid.major.x=element_line(colour="#D0D0D0",size=.25),
               panel.grid.major.y=element_line(colour="#D0D0D0",size=.5),
               panel.grid.minor=element_blank())
p <- p + theme(axis.ticks=element_blank())

# title
p <- p + ggtitle(plot_title) + 
  theme(plot.title=element_text(face = "bold", colour = "black", size = 28, lineheight = 0.75))

# axis & marks
p <- p + labs(y = "Local Revenues per Capita\n(constant 2016 COP, in thousands)", x="")

p <- p + theme(axis.text = element_text(size = 18, colour = "#535353", face="bold")) + 
  theme(axis.title = element_text(size = 20, colour = 'black', face="bold", vjust=.5))

p <- p + scale_y_continuous(breaks = y_brk, limits = c(min(y_brk), max(y_brk)) )
p <- p + scale_x_continuous(breaks = x_brk, limits = c(min(x_brk) - .5, max(x_brk) +.5) )

p <- p + geom_hline(yintercept = 0, size = 1.2, colour = "#535353")

# content
p <- p + geom_jitter( size = 2.5, width = .2, height = 0, alpha = 0.35, fill = "red", colour = "red")
p <- p + geom_smooth( method = 'loess', se = F, colour = 'blue')


# Keep plot
p1 <- p


rm( p, plot_title, y_brk, x_brk)



### Figure (2)
### Concentration of Revenues (Gini)

## Axis & Titles
plot_title <- 'Geographical Concentration\n'

y_brk <- seq(0, 1, length.out = 6)
x_brk <- seq(1986, 2002, length.out = 5)


## Plot
## Script

# ggplot element
p <- ggplot( data = plot_dat %>% filter(!is.na(local_rev_gini) ), 
             aes(x = year, y = local_rev_gini ))

# region
p <- p + theme_bw()
p <- p + theme(panel.border=element_rect(colour="white"))

# grid
p <- p + theme(panel.grid.major.x=element_line(colour="#D0D0D0",size=.25),
               panel.grid.major.y=element_line(colour="#D0D0D0",size=.5),
               panel.grid.minor=element_blank())
p <- p + theme(axis.ticks=element_blank())

# title
p <- p + ggtitle(plot_title) + 
  theme(plot.title=element_text(face = "bold", colour = "black", size = 28, lineheight = 0.75))

# axis & marks
p <- p + labs(y = "Concentration of Local Revenues\n(Gini Index)", x="")

p <- p + theme(axis.text = element_text(size = 18, colour = "#535353", face="bold")) + 
  theme(axis.title = element_text(size = 20, colour = 'black', face="bold", vjust=.5))

p <- p + scale_y_continuous(breaks = y_brk, limits = c(min(y_brk), max(y_brk)) )
p <- p + scale_x_continuous(breaks = x_brk, limits = c(min(x_brk) - .5, max(x_brk) +.5) )

p <- p + geom_hline(yintercept = 0, size = 1.2, colour = "#535353")

# content
p <- p + geom_jitter( size = 2.5, width = .2, height = 0, alpha = 0.35, fill = "red", colour = "red")
p <- p + geom_smooth( method = 'loess', se = F , colour = 'blue')


# Keep plot
p2 <- p


rm( p, plot_title, y_brk, x_brk)


### Save
### Figures

# combine plots
p <- cowplot::plot_grid( p1, p2)


# save plot
ggsave(paste(plot_dir, 'Figure II-1.png', sep = "/"), p, dpi = 800, width = 15, height = 7.5)



rm( plot_dat, p, p1, p2 )


