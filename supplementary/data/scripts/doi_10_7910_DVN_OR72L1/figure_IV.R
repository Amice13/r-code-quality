# ==========================================
# Description: Create Figure IV
# ==========================================

### ========================================
### Set Up =================================
### ========================================
# Set working directory to the directory to which this file is saved
setwd(dirname(getActiveDocumentContext()$path))

# Load Packages
source("requirements.R")

# Import Graph Themes
source("graph_themes.R")


### ========================================
### Custom Functions =======================
### ========================================
Get_Skin_Density <- function(data){
  p <- ggplot(data) + geom_density(aes(x=skin_tint, color=collection))
  density <- ggplot_build(p)$data[[1]]
  density <- density[,c("y","x", "group")]
  colnames(density) <- c("density","skin_tint","collection")
  return(density)
}

Graph_Skin_Density <- function(data, colors="polychromatic", grayscale=F){
  # Subset data to relevant image colors
  data %<>% subset(image_color == colors)
  
  # Find mean skin tint for a given image color by collection
  mean <- data %>% group_by(collection) %>% summarize(avg=mean(skin_tint))
  
  if(grayscale){
    color_theme <- miie$bw_palette
  } else {
    color_theme <- miie$color_blind_palette
  }
  
  # Create skin tint density for each collection
  mainstream <- Get_Skin_Density(subset(data, collection == "Mainstream")) 
  diversity <- Get_Skin_Density(subset(data, collection == "Diversity"))
  
  p <- ggplot(data, aes(y=density, x=skin_tint)) +
    
    # Fill Density
    geom_segment(data=mainstream, aes(color=skin_tint, xend=skin_tint, yend=0)) +
    geom_segment(data=diversity, aes(color=skin_tint, xend=skin_tint, yend=0)) +
    
    # Draw Density
    geom_line(data=mainstream, color=color_theme[1], size=1.5) +
    geom_line(data=diversity, color=color_theme[2], size=1.5) +
  
    
    # Label Density 
    geom_label(data=mean, x=85, y=0.0125, label="Mainstream", color=color_theme[1], fontface="bold") +
    geom_label(data=mean, x=10, y=0.01, label="Diversity", color=color_theme[2], fontface="bold") +
    
    # Format Graph
    theme(legend.position="none", axis.ticks.y=element_blank(), axis.text.y=element_blank()) +
    xlab(miie$arrow_label) + ylab("Density") + 
    
    # Add Dashed Lines for Averages
    geom_vline(data=mean, aes(group=collection, xintercept=avg), 
               color=color_theme[1:2], size=1, linetype="dashed")
  
  # Color Density with a Gradient Based on Image Color
  if(colors == "polychromatic" | colors == "no_monochromatic"){p <- p + scale_color_gradientn(colors=miie$polychromaticgradient, name=miie$arrow_label)}
  if(colors == "monochromatic" | grayscale){p <- p + scale_color_gradientn(colors=miie$monochromaticgradient, name=miie$arrow_label)}
  if(colors == "non_typical"){p <- p + scale_color_gradientn(colors=miie$non_typicalgradient, name=miie$arrow_label)}
  
  
  return(p)
}

Graph_Terciles_by_Decade <- function(data, colors="polychromatic", grayscale=F){
  
  # Set bin colors
  if(grayscale){bin_colors <- miie[["monochromatic"]]} else {bin_colors <- miie[[colors]]}
  names(bin_colors) <- c("1st","2nd","3rd")
  
  # Convert book_level data from long to wide
  data <- data[,c("collection","decade",paste0("pct_",colors,c("_1st","_2nd","_3rd")))]
  data %<>% 
    gather("terciles","pct", -c(collection,decade)) %>%
    mutate(terciles = str_replace(terciles, paste0("pct_",colors,"_"), ""))
  
  
  # Average over all books in a decade
  data %>%
    group_by(decade, collection, terciles, .drop = F) %>%
    summarise(mean_pct = mean(pct, na.rm = T)) %>%
    subset(!is.na(mean_pct)) %>%
    
    # Graph
    ggplot(aes(x=decade, y=mean_pct, fill=terciles)) +
    geom_area(size=0.5, colour="white") +
    ylab("") + xlab("Decades") +
    scale_y_continuous(labels = percent_format()) +
    facet_grid(cols=vars(collection)) +
    theme(legend.title=element_text()) +
    scale_fill_manual(values = rev(bin_colors), name = "Terciles")
}

Graph_Terciles_by_Collection <- function(data, colors="polychromatic", grayscale=F){
  # Set bin colors
  if(grayscale){bin_colors <- miie[["monochromatic"]]} else {bin_colors <- miie[[colors]]}
  names(bin_colors) <- c("1st","2nd","3rd")
  
  # Convert book_level data from long to wide
  data <- data[,c("collection","decade",paste0("pct_",colors,c("_1st","_2nd","_3rd")))]
  data %<>% 
    gather("terciles","pct", -c(collection,decade)) %>%
    mutate(terciles = str_replace(terciles, paste0("pct_",colors,"_"), ""))
  
  # Average over all books in a collection
  data %>%
    group_by(collection, terciles, .drop = F) %>%
    summarise(mean_pct = mean(pct, na.rm = T)) %>%
    subset(!is.na(mean_pct)) %>%
  
  # Graph
  ggplot(aes(x=collection, y=mean_pct, fill=terciles)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "bottom") +
  theme(legend.title=element_text(), 
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_manual(values = rev(bin_colors), name = "Terciles") +
  geom_text(aes(label=percent(mean_pct,1)),  color="black", fontface="bold", 
            position = position_dodge(width = .9), vjust = -0.3, size=2.5) +
  ylim(0,1)
}


### ========================================
### Data Prep ==============================
### ========================================
load("../data/representation_data.Rdata")


### ========================================
### Graph (Panel a) ========================
### ========================================
image_level_data %>%
  subset(collection %in% c("Mainstream", "Diversity")) %>%
  Graph_Skin_Density(grayscale=F)
ggsave("../figures/figure_IV_a.png", units="in", height=4,width=5)


image_level_data %>%
  subset(collection %in% c("Mainstream", "Diversity")) %>%
  Graph_Skin_Density(grayscale=T)
ggsave("../figures/grayscale_figure_IV_a.png", units="in", height=4,width=5)



### ========================================
### Graph (Panel b) ========================
### ========================================
book_level_data %>%
  subset(collection %in% c("Mainstream", "Diversity")) %>%
  Graph_Terciles_by_Decade(grayscale=F)
ggsave("../figures/figure_IV_b.png", units="in", height=4, width=5)

book_level_data %>%
  subset(collection %in% c("Mainstream", "Diversity")) %>%
  Graph_Terciles_by_Decade(grayscale=T)
ggsave("../figures/grayscale_figure_IV_b.png", units="in", height=4, width=5)



### ========================================
### Graph (Panel c) ========================
### ========================================
Graph_Terciles_by_Collection(book_level_data, grayscale=F)
ggsave("../figures/figure_IV_c.png", units="in", height=4, width=8.5)

Graph_Terciles_by_Collection(book_level_data, grayscale=T)
ggsave("../figures/grayscale_figure_IV_c.png", units="in", height=4, width=8.5)



