### ==========================================================================================================
### Set the Graph Themes and Colors
### ==========================================================================================================
collections <- c("Mainstream", "Diversity", "People of Color", "African American", 
                "Ability", "Female", "LGBTQIA+", "Non-Winners","Census")

theme_set(theme_bw() + 
            theme(plot.title = element_text(hjust = 0.5), # Center Graph Title
                  # Text
                  text = element_text(size=11, color="black"), # Font
                  axis.text = element_text(size=11),
                  axis.title = element_text(size=11, face="bold"),
                  # Set Facet Theme
                  strip.background = element_rect(fill="grey90", colour = "grey90"),
                  strip.text = element_text(face="bold", size=12),
                  # Remove Axis Lines
                  panel.border = element_blank(), 
                  # Remove panel grid lines
                  # panel.grid.minor = element_blank(),
                  panel.grid.minor = element_line(colour = "grey65"), # Grid Color
                  panel.grid.minor.x = element_blank() ,
                  # explicitly set the horizontal lines (or they will disappear too)
                  panel.grid.minor.y = element_line( size=.25, color="gray75", linetype = "longdash"),
                  panel.grid.major = element_blank(),
                  # Remove panel background
                  panel.background = element_blank(),
                  # Change legend 
                  legend.position = "bottom",
                  legend.text = element_text(size=11),
                  legend.title = element_blank()))

miie <- list()

# Color Blind Palette based on https://jfly.uni-koeln.de/color/
miie$color_blind_palette <- c("#CC79A7", #pink
                              "#0072B2", #blue
                              "#D55E00", #orange
                              "#009E73", #green
                              "#888888", #grey
                              "#56B4E9", #light blue
                              "#F0E142", #yellow
                              "#E69F00", #light orange
                              "#E69F00") #light orange

# Alternative Palette
miie$alternative_palette <- c("#25f560", "#a970ff", "#027523", "#623d99", "#b344ab")

# Grayscale Palette
miie$bw_palette <- c("gray0", "gray70", "gray50","gray40", "gray30", "gray20", "gray0","gray40","gray40") 


names(miie$color_blind_palette) <- collections
names(miie$bw_palette) <- collections

miie$colors <- list(scale_color_manual(values=miie$color_blind_palette),
                    scale_fill_manual(values=miie$color_blind_palette),
                    guides(fill=guide_legend(title=NULL)))

miie$bw <- list(scale_color_manual(values=miie$bw_palette),
                    scale_fill_manual(values=miie$bw_palette),
                    guides(fill=guide_legend(title=NULL)))

miie$alternate_colors <- list(scale_color_manual(values=miie$alternative_palette),
                             scale_fill_manual(values=miie$alternative_palette),
                             guides(fill=guide_legend(title=NULL)))

miie$sizes <- c(2,4,6,2,3,4,5,2,2)
names(miie$sizes) <- collections
miie$shape <- c(16,17,8,16,1,18,0,0,0)
names(miie$shape) <- collections

miie$lines <- list(scale_linetype_manual(values=c("solid", "solid", "dashed", "solid", "solid", "blank", "blank", "solid", "solid")),
                   scale_size_manual(values=miie$sizes),
                   scale_shape_manual(values=miie$shape),
                   theme(legend.key.size = grid::unit(2, "lines")))

miie$shapes <- list(scale_shape_manual(values=miie$shape))


miie$arrow_label <- expression(bold("("%<-% "Darker )     Perceptual Tint     ( Lighter" %->%")"))
miie$narrow_labels <- c("Mainstream"="Mainstream", "Diversity"="Diversity", "People of Color"="People\nof Color", "African American"="African\nAmerican", "Ability"="Ability", "Female"="Female", "LGBTQIA+"="LGBTQIA+")
miie$narrow_levels <- c("Mainstream"="Mainstream", "Diversity"="Diversity", "People\nof Color"="People of Color", "African\nAmerican"="African American", "Ability"="Ability", "Female"="Female", "LGBTQIA+"="LGBTQIA+")
miie$labeler <- function(variable,value){
  if (variable=='collection'){
    return(miie$narrow_labels[value])
  } else {
    return(as.character(value)) 
  }
}



# Dark, Medium, Light
miie$polychromatic <- c("#582818", "#A27D5A", "#D5C09C")
miie$monochromatic <- c("#323232", "#7E7E7E", "#BBBBBB")
miie$non_typical <- c("#1C2F6F", "#1681B5", "#C8E6F0")

miie$polychromaticgradient <- c("#000000", "#3F1B17", "#633A2C", "#8C5E43","#B17F59", "#D0AA80", "#EAD3A8", "#FFFFFF")
miie$monochromaticgradient <- c("#000000", "#202020", "#404040", "#606060", "#888888", "#B5B5B5", "#E5E5E5", "#FFFFFF")
miie$non_typicalgradient <- c("#000000", "#13264B", "#19386A", "#2D639E", "#3297BE", "#83BDD5", "#B5F7F7", "#FFFFFF")

