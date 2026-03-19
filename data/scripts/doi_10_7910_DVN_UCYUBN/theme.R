# Based on theme_solarized_2
require(extrafont)
require(scales)
require(grid)
require(ggthemes)

xfimtheme_data <- list(regular=
    c(
      green         = rgb(0,   148, 64, max=255),
      red           = rgb(255,   0,  0, max=255),
      lightblue     = rgb(0,   158, 254, max=255),
      #green         = rgb(137, 186,  22, max=255),
      purple        = rgb(144, 104, 190, max=255),
      orange        = rgb(255, 215, 0,   max = 255),
      lightgrey     = rgb(112, 113, 115, max=255),
      verylightgrey = rgb(230, 230, 230, max=255),
      grey          = rgb(85,   85,  85, max=255),
      green         = rgb(137, 186,  22, max=255),
      white         = rgb(255, 255, 255, max=255),
      black         = rgb(  0,   0,   0, max=255)
))

xfim_pal <- function(palette="regular") {
  values <- xfimtheme_data[[palette]]
  n <- length(values)
  manual_pal(unname(values[1:n]))
}

scale_colour_xfim <- function(palette="regular", ...) {
  discrete_scale("colour", "xfim", xfim_pal(palette), ...)
}

scale_color_xfim <- scale_colour_few

scale_fill_xfim <- function(palette="regular", ...) {
  discrete_scale("fill", "xfim", xfim_pal(palette), ...)
}


# Todo add qualitative palette
scale_fill_solarized <- function(...) {
  discrete_scale("fill", "xfim", solarized_pal(accent), ...)
}

theme_xfim <- function(base_size = 10, base_family="Libre Franklin") {
  green         <- rgb(0,   255,  0, max=255)
  red           <- rgb(255,   0,  0, max=255)
  lightblue     <- rgb(0,   158, 254, max=255)
  lightgrey     <- rgb(112, 113, 115, max=255)
  verylightgrey <- rgb(230, 230, 230, max=255)
  grey          <- rgb(85,   85,  85, max=255)
  #green         <- rgb(137, 186,  22, max=255)
  white         <- rgb(255, 255, 255, max=255)
  black         <- rgb(  0,   0,   0, max=255)
  purple        <- rgb(144, 104, 190, max=255)
  ret <- (theme_foundation(base_size = base_size, base_family = base_family) +
          theme(text = element_text(color=lightgrey),
                title = element_text(color=black),
                line = element_line(color=lightgrey),
                rect = element_rect(fill=white, color=NA),
                axis.ticks = element_line(color=grey),
                axis.line = element_line(color=lightgrey, linetype=1),
                axis.title.y = element_text(angle = 90),
                legend.background = element_rect(fill=NULL, color=NA),
                legend.key = element_rect(fill=NULL, colour=NULL, linetype=0),
                #                panel.background = element_rect(fill=NA, colour=NA),
                #panel.background = element_rect(fill=NA, colour=NA),
                panel.background = element_blank(),
                panel.border = element_blank(),
                panel.spacing.x = unit(0.5, "lines"),
                panel.spacing.y = unit(0.5, "lines"),
                #panel.grid = element_line(color=NA),
                panel.grid = element_blank(),
                #                panel.grid.major = element_line(color=verylightgrey),
                #                panel.grid.minor = element_line(color=verylightgrey, size=0.25),
                #strip.background = element_rect(fill=green, size=3),
                strip.background = element_rect(fill=verylightgrey, size=3),
                strip.text.x = element_text(colour=black),
                strip.text.y = element_text(colour=black),
                #plot.background = element_rect(fill=NA, colour=NULL, linetype=0)
                plot.background = element_blank()
          ))
  ret
}

scale_colour_discrete <- scale_colour_xfim
scale_fill_discrete <- scale_fill_xfim

update_geom_defaults("point",   list(colour = xfimtheme_data[["regular"]]["black"]))
update_geom_defaults("line",    list(colour = xfimtheme_data[["regular"]]["black"]))
update_geom_defaults("segment",    list(colour = xfimtheme_data[["regular"]]["black"]))
update_geom_defaults("linerange",    list(colour = xfimtheme_data[["regular"]]["black"]))
update_geom_defaults("ribbon", list(
  colour = NA,
  fill = xfimtheme_data[["regular"]]["black"]))
#update_geom_defaults("bar", list(
#  colour = xfimtheme_data[["regular"]]["black"],
#  fill = xfimtheme_data[["regular"]]["black"]))
