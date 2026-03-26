# ==========================================
# Description: Create Figure V
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


Graph_Skin_Density <- function(data, colors="polychromatic", fill_var, grayscale=F){
  # Subset data to relevant image colors
  data %<>% subset(image_color == colors)
  
  if(colors == "polychromatic"){img <- img_poly}
  if(colors == "non_typical"){img <- img_non}
  if(colors == "monochromatic" | grayscale){img <- img_mono}
  
  p <- ggplot(data, aes_string(x="skin_tint", fill=fill_var))
  
  if(!grayscale){
    p <- p + annotation_custom(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc")), -Inf, Inf, -Inf, Inf)
  }
  
  p <- p + geom_density(alpha=0.75) +
    scale_y_continuous(limits=c(0,(max(Get_Skin_Density(data)$density) + 0.0095)), expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    xlab(miie$arrow_label) + ylab("Density") +
    guides(fill = guide_legend(NULL, order = 2, override.aes = list(size = 0))) +
    theme(strip.text.y = element_text(angle=0), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank())
  

  
  if(grayscale){
      p <- p +  scale_fill_manual(values=c("gray0", "gray70"))
    } else if(fill_var=="collection"){
      p <- p + miie$colors
    } else {
      p <- p + miie$alternate_colors
    }

  return(p)
}


### ========================================
### Data Prep ==============================
### ========================================
load("../data/representation_data.Rdata")

img_mono <- png::readPNG("../data/supplemental/monochromatic_gradient.png")
img_poly <- png::readPNG("../data/supplemental/polychromatic_gradient.png")
img_non <- png::readPNG("../data/supplemental/non_typical_gradient.png")

image_level_data %<>% subset(collection %in% c("Mainstream", "Diversity"))



### ========================================
### Graph (Panel a) ========================
### ========================================
image_level_data %>% Graph_Skin_Density(grayscale=F, fill_var="collection") + 
  facet_wrap(vars(race), 2)
ggsave("../figures/figure_V_a.png", units="in", height=6, width=8)

image_level_data %>% Graph_Skin_Density(grayscale=T, fill_var="collection") + 
  facet_wrap(vars(race), 2)
ggsave("../figures/grayscale_figure_V_a.png", units="in", height=6, width=8)



### ========================================
### Graph (Panel b) ========================
### ========================================
image_level_data %>% Graph_Skin_Density(grayscale=F, fill_var="gender") + 
  facet_grid(rows=vars(collection)) +
  theme(strip.text.y = element_text(angle=270))
ggsave("../figures/figure_V_b.png", units="in", height=5, width=4)

image_level_data %>% Graph_Skin_Density(grayscale=T, fill_var="gender") + 
  facet_grid(rows=vars(collection)) +
  theme(strip.text.y = element_text(angle=270))
ggsave("../figures/grayscale_figure_V_b.png", units="in", height=5, width=4)



### ========================================
### Graph (Panel c) ========================
### ========================================
image_level_data %>% Graph_Skin_Density(grayscale=F, fill_var="age_group") + 
  facet_grid(rows=vars(collection)) +
  theme(strip.text.y = element_text(angle=270))
ggsave("../figures/figure_V_c.png", units="in", height=5, width=4)

image_level_data %>% Graph_Skin_Density(grayscale=T, fill_var="age_group") + 
  facet_grid(rows=vars(collection)) +
  theme(strip.text.y = element_text(angle=270))
ggsave("../figures/grayscale_figure_V_c.png", units="in", height=5, width=4)


