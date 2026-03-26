# Policy+ Figure themes and other helpers 

# Define a custom ggplot2 theme for consistent visualization style.
# This ensures figures meet the standards described in the paper (Figures 3, 4, and 5).
policyplustheme_w_legend <- function(base_size = 14, base_family = ""){
  theme_sjplot() %+replace%
    theme(strip.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16, colour = "black"), # no hjust for centered
          strip.text.x = element_text( size=14, colour = "black"),
          axis.line.y.left = element_line(colour = "black"),
          axis.line.x.bottom  =   element_line(colour = "black"),
          axis.text = element_text( size=14, colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(  size=14,  colour = "black"),
          legend.title = element_text(  size=14,  colour = "black"),
          legend.text = element_text(  size=14,  colour = "black"), #
          legend.position = "bottom",
          plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
}
# policyplustheme_w_legend() 


policyplustheme_heterogeneous <- function(base_size = 14, base_family = ""){
  theme_sjplot(base_size = base_size ) %+replace%
    theme(strip.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16, colour = "black", hjust = 0.5), # hjust for centerede
          strip.text.x = element_text( size=14, colour = "black"),
          axis.line.y.left = element_line(colour = "black"),
          axis.line.x.bottom  =   element_line(colour = "black"),
          axis.text = element_text( size=14, colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(  size=14,  colour = "black"),
          legend.title = element_text(  size=14,  colour = "black"),
          legend.text = element_text(  size=14,  colour = "black"), #
          legend.position = "bottom",
          plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
}









policyplustheme_no_legend <- function(base_size = 14, base_family = ""){
  theme_sjplot() %+replace%
    theme(strip.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=16, colour = "black"),
          strip.text.x = element_text( size=14, colour = "black"),
          axis.line.y.left = element_line(colour = "black"),
          axis.line.x.bottom  =   element_line(colour = "black"),
          axis.text = element_text( size=14, colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(  size=14,  colour = "black"),
          legend.title = element_text(  size=14,  colour = "black"),
          legend.text = element_text(  size=14,  colour = "black"), #
          legend.position = "none",
          plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))
}

# policyplustheme_no_legend()

combine_and_make_same_y_axis <- function(plot1, plot2) {
  
  output <- plot1 + plot2 & scale_y_continuous(limits = c(min( layer_scales(plot1)$y$range$range, layer_scales(plot2)$y$range$range), max( layer_scales(plot1)$y$range$range, layer_scales(plot2)$y$range$range))) 
  output <- ggpubr::ggarrange(output, ncol = 1, nrow = 1, common.legend = TRUE, legend = "bottom")
  return(output)
}

# combine_and_make_same_y_axis(a,b)






require(tikzDevice)

options(tikzDocumentDeclaration = "\\documentclass[12pt]{article}")


fig2tex <- function(object, filename, width = 6.5, height = 6.5, sanitize=TRUE, standAlone = TRUE){
  tikz(filename = filename, width = width, height = height , sanitize=sanitize, standAlone = standAlone)
  print(object)  # Save the provided plot
  dev.off()
  object        # Return the plot object
}

combofig2tex <- function(object, filename, width = 16, height = 8.5, sanitize=TRUE, standAlone = TRUE){
  tikz(filename = filename, width = width, height = height , sanitize=sanitize, standAlone = standAlone)
  print(object)  # Save the provided plot
  dev.off()
  object        # Return the plot object
}
