# ==========================================
# Description: Create Figure III
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
All_Skin_Colors <- function(data, colors, grayscale=F){
  data %<>% subset(image_color == colors)
  
  if(grayscale){
    # Convert to grayscale
    data$hex %<>% sapply(function(x) {
      rgb_val <- col2rgb(x)
      gray_val <- weighted.mean(rgb_val, c(0.2989, 0.5870, 0.1140)) %>% as.integer()
      gray_hex <- sprintf("#%02X%02X%02X", gray_val, gray_val, gray_val)
      return(gray_hex)
    })
  }
  
  hex_colors <- c(unique(data$hex))
  names(hex_colors) <- c(hex_colors)
  
  data %>% 
    ggplot(aes(y=rgb_sd, x=skin_tint, color=hex)) + 
    geom_point() + 
    scale_y_continuous(breaks=pretty_breaks(4))  +
    scale_color_manual(values=hex_colors) +
    theme(legend.position = "none", strip.text.y = element_text(angle=0)) + 
    xlab(miie$arrow_label) + 
    ylab("Standard Deviation of (R,G,B)") + 
    facet_grid(rows=vars(collection), labeller=miie$labeler)
}



### ========================================
### Data Prep ==============================
### ========================================
load("../data/representation_data.Rdata")



### ========================================
### Graph ==================================
### ========================================
# Polychromatic, Color
All_Skin_Colors(image_level_data, colors="polychromatic",F)
ggsave("../figures/figure_III_a.png", units="in", height=8, width=6)

# Monochromatic, Color
All_Skin_Colors(image_level_data, colors="monochromatic",F)
ggsave("../figures/figure_III_b.png", units="in", height=8, width=6)

# Non Typical, Color
All_Skin_Colors(image_level_data, colors="non_typical",F)
ggsave("../figures/figure_III_c.png", units="in", height=8, width=6)

# Polychromatic, Grayscale
All_Skin_Colors(image_level_data, colors="polychromatic",T)
ggsave("../figures/grayscale_figure_III_a.png", units="in", height=8, width=6)

# Monochromatic, Grayscale
All_Skin_Colors(image_level_data, colors="monochromatic",T)
ggsave("../figures/grayscale_figure_III_b.png", units="in", height=8, width=6)

# Non Typical, Grayscale
All_Skin_Colors(image_level_data, colors="non_typical",T)
ggsave("../figures/grayscale_figure_III_c.png", units="in", height=8, width=6)






