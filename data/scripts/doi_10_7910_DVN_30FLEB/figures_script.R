# Plot a network graph of nodes with geographic coordinates on a map.
#
# Code format based on gist blog by: Markus Konrad, May 2018, below
# https://gist.github.com/internaut/a9a274c72181eaa7f5c3ab3a5f54b996
# 

# misc
library(assertthat)

# graphs
library(igraph)

# plotting
library(ggplot2)
library(ggraph)
library(ggrepel)
library(GGally)
library(RColorBrewer)
library(wesanderson)
library(ggnewscale)

# data science
library(tidyverse)
library(dplyr)
library(ineq)
library(forcats)

# data
library(rnaturalearth)
library(maps)
library(googlesheets4)

# mapping
library(sf)
library(tmap)
library(ggmap)


#### Data Preparation ####
# add tiny countries from NE data
tiny_countries <- ne_countries(type = 'tiny_countries', returnclass = 'sf',
                               country=c("Singapore", "Bahrain", "Malta")) %>%
  select(name) 

# Download disputed areas from Natural Earth data
# http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_disputed_areas.zip
# Unzip shapefile and update data location if needed
unzip("ne_10m_admin_0_disputed_areas 2.zip")
palestine <- read_sf("ne_10m_admin_0_disputed_areas/ne_10m_admin_0_disputed_areas.shp") %>%
  filter(str_detect(ADMIN, "Palesti")) %>%
  select(name = ADMIN) %>%
  group_by(name) %>% 
  summarise()

# Countries loaded from Natural Earth
world <- ne_countries(returnclass='sf', type = 'sovereignty') %>%
  select(name)

# Add tiny countries to the rest of the world.
tiny_countries <- tiny_countries[ ,colnames(tiny_countries) %in% colnames(world)]
world <- world[,colnames(world) %in% colnames(tiny_countries)] %>%
  rbind(tiny_countries, palestine) 

# Read data (change location)
indicators <- read_csv("indicators.csv")
indexes <- read_csv("index_countries.csv")
nodes <- read_csv("nodes.csv")
edges <- read_csv("edges.csv")
#sdg_labels <- read_csv("sdg_labels.csv")

# Create the igraph graph object
# You can activate the commented code and get an undirected network.
# If so, you must also remove "fill=Sector" from the plot. That section
# is also commented.
g <- graph_from_data_frame(indexes, vertices = nodes, directed = T) %>% 
  igraph::simplify(remove.multiple = F, remove.loops=T)

# # edges: extract edges
# edges <- as_long_data_frame(g)
# #write_csv(edges, "edges.csv")
# --------------------------------------------------------------------- #
# Create ggplot objects for network plot #####


# create a data frame for plotting the edges
# join with nodes to get start and end positions for each
# edge (x, y and xend, yend)

edges_for_plot <- edges %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat)

assert_that(nrow(edges_for_plot) == nrow(edges))

# Use the node degree for scaling the node sizes
# (Can change this to a different metric, if so desired.)
nodes$weight = degree(g)

# common plot theme
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = c(.1,.3), legend.background = element_blank()) +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "lightblue")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

# common polygon geom for plotting the country shapes
country_shapes <- geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group),
                               fill = "#ffffff", color = "#515151", size = 0.15)
# common coordinate system for all the following plots
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

# ---------------------------------------- #
# igraph+map solution: ggplot + ggmap only #
# ---------------------------------------- #

# plot with scaled edge widths and node sizes
# this will fail because we can only use the "size" aesthetic twice
nodes <- nodes %>%
  mutate(plot_name = if_else(weight > 3.5, name, ""),
         group = as.character(group)) 


#### Figure 1 ####
ggplot(nodes) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend  # draw edges as arcs
                 # comment out this line if you made the graph undirected
                 , linetype = sector
                 ),
             data = edges_for_plot, curvature = 0.33, alpha=0.5, 
             arrow=arrow(length = unit(.2,"cm"), type="closed")) +
  scale_linetype_manual(name="Index creator sector", values=c("dashed", "solid", "dotted"))+
  scale_color_manual(values=c("#BDBDBD"))+
  labs(color = "Index Sector") +
  scale_size_continuous(guide = FALSE, range = c(0.01, 1)) +   # scale for edge widths
  geom_point(aes(x = lon, y = lat, size = weight, fill=group), # draw nodes
             shape = 21, color = 'black', stroke = 0.5) +
  scale_fill_manual(values = c("#1DACE8", "#F24D29", "#1C366B", "#E5C4A1"))+
  labs(fill="", size = "Degree") +

  mapcoords + maptheme + 
  theme(legend.position = "bottom", legend.background = element_blank(), legend.text=element_text(size=11)) +
  
  scale_size_continuous(guide = "none", range = c(2, 6)) +      # scale for node size
  guides(fill = guide_legend(override.aes = list(size=5)))+
  # geom_text(aes(x = lon, y = lat, label = plot_name),        
  #           hjust = 0, nudge_x = 1, nudge_y = 4,
  #           size = 3, color = "white", fontface = "bold") +
  geom_label_repel(aes(x = lon, y=lat, label=factor(plot_name)), fontface="bold", force=2) + # draw text labels
  ggtitle("Countries included in Review") 



# figure 2
edges %>% select(scope_group, sector) %>% 
  #unite("Origin to Scope", origin_group:scope_group, sep="->" ) %>% 
  ggplot(aes(scope_group, fill=sector)) + 
  geom_bar() + 
  scale_fill_manual(values = c("#1DACE8", "#F24D29", "#1C366B", "#E5C4A1"))+
  labs(fill="") +
  ylab("Number of Indexes") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = c(.8, .8)) + 
  theme(axis.text = element_text(size=12), legend.title = element_text(size=14), legend.text=element_text(size=14), 
        legend.position = c(0.75, 0.8)) 


##### Prep indicators_spec ####
indicators_spec <- indicators %>%
  dplyr::select(Sector, 'Issue Area') %>%
  filter(!is.na(`Issue Area`) & 
           `Issue Area` != "Funding/Capacity for Environmental Management") %>%
  mutate(`Sector` = 
           str_replace_all(`Sector`, 
                           c("Think Tank" = "NGO", 
                             "Academic" = "NGO", 
                             "Intergovernmental"= "Government",
                             "Development Agency" = "Government"))) %>%
  group_by(Sector) %>%
  count(`Issue Area`)

#### Figure 2 ####
cairo_pdf("Figure2.pdf", width=8.5, height=6)
indicators_spec %>% 
  #####
  ggplot(aes(x=reorder(`Issue Area`, n, sum), n, fill=`Sector`), na.rm=T) +
  geom_bar(stat="identity", na.rm = TRUE) +
  scale_color_ordinal(direction = -1) +
  scale_fill_manual(values = c("#1DACE8", "#F24D29", "#1C366B", "#E5C4A1")) +
  coord_flip() +
  ylab("Number of indicators") +
  xlab("") +
  theme_UESI() +
  theme(axis.text.x = element_text(size=12), axis.title.y=element_text(size=14),
        legend.position = "bottom", axis.text = element_text(size=12), 
        legend.text=element_text(size=12))  
dev.off()

indicators_spec <- indicators %>%
  dplyr::select('Sector', 'Issue Area') %>%
  filter(!is.na(`Issue Area`) & `Issue Area` != "Funding/Capacity for Environmental Management") %>%
  mutate(`Sector` = str_replace_all(`Sector`, c("Think Tank" = "NGO", "Academic" = "NGO", "Development Agency" = "Government", "Intergovernmental" = "Government")))%>%
  group_by(Sector) %>%
  count(`Issue Area`)

#### Figure 3 ####
indicators_spec %>% 
  ggplot(aes(x=reorder(`Issue Area`, n, sum), n, fill=`Sector`), na.rm=T) +
  geom_bar(stat="identity", na.rm = TRUE) +
  scale_color_ordinal(direction = -1) +
  scale_fill_manual(values = c("#1DACE8", "#F24D29", "#1C366B", "#E5C4A1"))+
  coord_flip()+
  ylab("Number of indicators")+
  xlab("")+
  theme_UESI() +
  theme(axis.text.x = element_text(size=12), axis.title.y=element_text(size=14),
        legend.position = "bottom", axis.text = element_text(size=12), 
        legend.text=element_text(size=12))  


###############
#### Figure 4 ####
sdg_labels <- c(`11.B` = "Target 11.B: Implement policies for inclusion, \nresource efficiency and disaster risk reduction",
                `11.B.1` = "Indicator 11.B.1: Integrated disaster risk management",
                `11.A` = "Target 11.A: Strong national and regional development planning",
                `11.7.2` = "Indicator 11.7.2: Safe spaces in cities for women and children, older persons \nand persons with disabilities",
                `11.7.1` = "Indicator 11.7.1: Provide universal access to safe, inclusive \nand accessible, green and public spaces",
                `11.7` = "Target 11.7: Provide access to safe and inclusive green and public spaces",
                `11.6.2` = "Target 11.6.2: Urban air pollution",
                `11.6.1` = "Target 11.6.1: Solid waste management",
                `11.6` = "Target 11.6: Reduce the environmental impacts of cities",
                `11.5` = "Target 11.5: Reduce the adverse effects of natural disasters",
                `11.4.1` = "Indicator 11.4.1: Protecting cultural heritage",
                `11.4` = "Target 11.4:Protect the world's cultural and natural heritage",
                `11.3.2` = "Indicator 11.3.2: Urban planning management",
                `11.3.1` = "Indicator 11.3.1: Sustainable urbanization rates",
                `11.3` = "Target 11.3: Inclusive and sustainable urbanization",
                `11.2.1` = "Indicator 11.2.1: Public transport access",
                `11.2` = "Target 11.2: Affordable and sustainable transport systems",
                `11.1.1` = "Indicator 11.1.1: Safe and affordable housing",
                `11.1` = "Target 11.1: Safe and affordable housings"
)

#######
indicators %>%
  mutate(SDG = as.character(`SDG-11 Target and Indicator`),
         Target = if_else(nchar(SDG) == 4, T , F)) %>%
  select(SDG, Target) %>%
  filter(`SDG`!="NULL")  %>% 
  # plot
  ggplot() +
  geom_histogram(aes(reorder(SDG, desc(SDG))), stat="count", fill="#3B9AB2") +
  scale_x_discrete(labels=sdg_labels)+
  scale_y_continuous(breaks=seq(0,70,10))+
  #facet_wrap(SDG,Target) + 
  coord_flip() +
  theme_UESI() +
  ylab("Number of indicators") +
  xlab("") 
dev.off()

#### Figure 5 ####
pal <- rev(c('#ffffcc','#c2e699','#78c679','#31a354','#006837'))

indicators_spec <- indicators %>%
  filter(!is.na(`Issue Area`) & !is.na(`Target Quality`)) %>%
  mutate(#`Issue Area` = fct_infreq(`Issue Area`),
    `Target Quality` = str_replace(string = `Target Quality`, 
                                   pattern = "No target", "No Target"),
    `Target Quality` = 
      fct_relevel(`Target Quality`, rev(c("No Target", "Baseline Only",
                                          "Has Target but no Baseline",
                                          "Directional (increase/decrease)",
                                          "Target with Baseline")))) %>%
  group_by(`Issue Area`, `Target Quality`) %>%
  tally(name = "count") %>% 
  mutate(pct = count / sum(count))

indicators_spec %>% mutate(loc = rev(cumsum(rev(pct)))) %>%
  # arrange(desc(n)) %>%
  ggplot(aes(x=`Issue Area`, count, fill=`Target Quality`)) +
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(x=`Issue Area`, y=loc,
                label = prettyNum(count, big.mark = ",")), color="#FFFFFF",
            vjust = 1,  size = 5, show.legend = F, family="Myriad Pro Light") +
  scale_color_ordinal(direction = -1) +
  theme_UESI() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = c(.8, .8))  +
  scale_fill_manual(name="", values = wes_palette("Zissou1", n = 5))+
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  ylab("Percentage of indicators")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.title.y=element_text(size=14),
        legend.position = "bottom", axis.text = element_text(size=12), 
        legend.text=element_text(size=12))  

###### 
# Figure 6

indicators %>%
  filter(!is.na(`Issue Area`)) %>%
  filter(!is.na(Equity)) %>%
  group_by(`Issue Area`, Equity) %>%
  tally(name = "count") %>%
  mutate(pct = count / sum(count) - .017,
         loc = rev(cumsum(rev(pct))), 
         
         Equity = fct_rev(Equity)) %>%
  ungroup() %>%
ggplot(aes(`Issue Area`, count, fill=Equity)) +
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(x=`Issue Area`, y=loc,
                label = prettyNum(count, big.mark = ",")), 
            size = 4, color="#FFFFFF", family="Myriad Pro Light", show.legend = F) +
  scale_fill_manual(name="", values=c("#3B9AB2", "#F21A00")) +
  scale_y_continuous(labels = scales::percent) +
  xlab("")+
  ylab("Percentage of indicators")+
  theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text = element_text(size=12),
        legend.position = "bottom", legend.text=element_text(size=12)) 


