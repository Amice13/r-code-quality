theme_jake <- function(base_size = 10, base_family = "sans",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      legend.position   = 'none',
      axis.title.x=element_blank(),
      #strip.text = element_text(size = 13),
      complete = TRUE
    )
}

theme_jake2 <- function(base_size = 10, base_family = "sans",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      legend.position   = 'none',
      #axis.title.x=element_blank(),
      #strip.text = element_text(size = 13),
      complete = TRUE
    )
}

theme_jake_dens <- function(base_size = 12, base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      legend.position   = 'none',
      axis.title.x=element_blank(),
      strip.text = element_text(size = 8),
      complete = TRUE
    )
}

theme_jake_scatter <- function(base_size = 15, base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_rect(color='black', fill=NA, size = 1),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      legend.position   = 'none',
     axis.title.x=element_blank(),
     axis.title.y=element_blank(),
     axis.text = element_blank(),
      complete = TRUE
    )
}


theme_jake_noY <- function(base_size = 14, base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      legend.position   = 'none',
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      complete = TRUE
    )
}

theme_jake_noaxes <- function(base_size = 13, base_family = "",
                           base_line_size = base_size / 22,
                           base_rect_size = base_size / 22) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      legend.position   = 'none',
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title = element_blank(),
      complete = TRUE
    )
}

theme_jake_tile2 <- function(base_size = 10, base_family = "sans",
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      plot.title = element_text(size = 15),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      legend.position   = 'none',
      # axis.title.x=element_blank()
      
      complete = TRUE
    )
}

theme_jake_tile <- function(base_size = 10, base_family = "sans",
                            base_line_size = base_size / 22,
                            base_rect_size = base_size / 22) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      plot.title = element_text(size = 15),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      legend.position   = 'none',
      axis.title.x=element_blank(),
      complete = TRUE
    )
}
