### The Burden of Election Logistics
### Election Ballots and the Territorial Influence of Party Machines in Colombia

### The Journal of Politics, forthcoming.

### Santiago Alles  <santiago.alles@voxel-consulting.com>
### Monica Pachon   <mopachon@uniandes.edu.co>
### Manuela Munoz   <manuela.munozf@tamu.edu>

### updated: 04/20/2020


### Loading
### Packages

require(rgdal)
require(maptools)

require(ggplot2)
require(cowplot)

#require(ggsci)

#require(tidyverse)



### Loading
### Shapes

dept_geo = readOGR(dsn = dir , layer= 'Antioquia' )                       # load shapes

dept_points = fortify(dept_geo, region = 'Codigo_Dan' )                   # melts the polygons into points
dept_border = fortify(dept_geo, region = 'Departamen' )

names(dept_points)[names(dept_points) == 'id'] <- 'DANE_mun_code'



### Map
### function

map_FUN <- function( data, title, mun_lim, dept_lim, legend = F) {
  
  ## Combine:
  ## HCwt idx & Geo-location
  
  merge( data %>% dplyr::select( department_name, department_code, municipality_name, municipality_code, LQ_idx, HC_idx, HCwt_idx),
         dept_points %>% dplyr::select( DANE_mun_code, long, lat, order, hole, piece, group ),
         by.x = 'municipality_code', by.y = 'DANE_mun_code', all.y = T ) %>% 
    arrange(order) %>% 
    tbl_df() -> data
  
  
  ## Plot
  ## Script
  
  # ggplot element
  map <- ggplot(data, aes(long, lat, group = group))
  
  # map region
  map <- map + theme_bw()
  map <- map + theme(panel.border = element_rect(colour="white"))
  
  # grid
  map <- map + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() )
  map <- map + theme(axis.ticks = element_blank())
  
  # axis text & marks
  map <- map + 
    ggtitle(label = title) + 
    theme(title = element_text(size = 28, colour="black", face = 'bold' )) + 
    labs(y = "", x="")
  
  map <- map + theme(axis.text = element_blank() ) + theme(axis.title = element_blank())
  
  # plot legend
  if(legend) {
    
    map <- map + theme( legend.background = element_rect(fill="#FFFFFF"),
                        legend.text  = element_text(size = 18, colour = "black" ),
                        legend.title = element_text(size = 22, colour = "black", face = "bold"),
                        legend.key = element_rect(fill="#FFFFFF", colour = NA),
                        legend.key.height = unit(0.6, "cm"), legend.key.width = unit(0.65, "cm"),
                        legend.spacing.x = unit(0.3, "cm"),
                        legend.position = c(0.95, 0.15))
    
  } else {
    
    map <- map + theme(legend.position="none")
    
  }
  
  # scale gradient
  map <- map + scale_fill_gradient( breaks = seq(-20, 60, 20), limits = c(-20, 60),      ## midpoint = 0, 
                                    low = 'white', high = 'black' , na.value = 'white',  ## dark green "#006837"
                                    name = 'HC-wt\nIndex' )
  
  # plot content
  map <- map + geom_polygon( aes(fill = HCwt_idx)) +
    geom_path(color="white", size = .15) +
    coord_map(projection = "gilbert")
  
  map <- map + geom_polygon(data = dept_border, fill = NA, color = 'black', size = .3)
  
  
  ## Function
  ## Output
  
  return(map)
  
}



### Build
### Maps

# Department: Antioquia
# Years: 1986, 2002
# Party: Liberal
# Faction ID: 4, 164


## Panel (i)

plot_title <- 'Antioquia, 1986'
map_data <- LQ_idx_store %>% filter(year == 1986)

map_1 <- map_FUN(map_data, plot_title, dept_points, dept_border, legend = F)

rm(plot_title, map_data)


## Panel (ii)

plot_title <- 'Antioquia, 2002'
map_data <- LQ_idx_store %>% filter(year == 2002)

map_2 <- map_FUN(map_data, plot_title, dept_points, dept_border, legend = T)

rm(plot_title, map_data)


## combine plots & save
map <- cowplot::plot_grid( map_1, map_2)

ggsave(paste(plot_dir, 'Figure 4.png', sep = "/"), map, dpi = 800, width = 15, height = 7.5)


rm( map_1, map_2, map,
    map_FUN,
    dept_geo, dept_points, dept_border)


