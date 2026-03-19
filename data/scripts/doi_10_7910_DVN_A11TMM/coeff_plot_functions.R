
#### coeff. plot adjusted functions for text sizes

add_brackets <- function(p, brackets, face = "italic") {
  y_ind <- term <- estimate <- ymax <- ymin <- x <- NULL # not functional, just for CRAN check
  
  coef_layer <- 0
  repeat {
    coef_layer <- coef_layer + 1
    if ("x" %in% names(layer_data(p, i = coef_layer))) break
  }
  
  if (p$args$style == "dotwhisker") {
    pd <- left_join(p$data %>% mutate(xx = signif(estimate, 9)),
                    layer_data(p, i = coef_layer) %>% mutate(xx = signif(x, 9)), by = "xx") %>%
      left_join(layer_data(p, i = coef_layer - 1),
                by = c("colour", "y", "group", "PANEL", "ymin", "ymax", "xmax", "size", "alpha"))
  } else {
    pd <- left_join(p$data %>% mutate(xx = signif(estimate, 9)),
                    layer_data(p, i = coef_layer) %>% mutate(xx = signif(x, 9)), by = "xx")
    pd <- pd %>%
      mutate(ymin = y_ind,
             ymax = y_ind)
  }
  overhang <- max(pd$y_ind)/30
  overhang <- ifelse(overhang > .23, .23, overhang)
  farout <- ifelse(p$args$style == "distribution", max(pd$x, na.rm = TRUE) + 100, max(pd$xmax, na.rm = TRUE) + 100)
  p1 <- p + theme(plot.margin = unit(c(1, 1, 1, -1), "lines")) + ylab("")
  
  if (!is.list(brackets)) stop('Error: argument "brackets" is not a list')
  
  
  draw_bracket_label <- function(x, f = face) {
    top <- pd[which((pd$term == x[2] | pd$term == x[3]) & !is.na(pd$estimate)), "ymax"] %>% max()
    bottom <- pd[which((pd$term == x[2] | pd$term == x[3]) & !is.na(pd$estimate)), "ymin"] %>% min()
    shift <- max(abs(top - round(top)), abs(round(bottom) - bottom))
    top <- round(top) + shift
    bottom <- round(bottom) - shift
    
    annotation_custom(
      grob = grid::textGrob(label = x[1], gp = grid::gpar(cex = 1, fontface = f,fontsize = 12), rot = 90),
      xmin = farout, xmax = farout,
      ymin = (top + bottom)/2, ymax = (top + bottom)/2)
  }
  
  draw_bracket_vert <- function(x, oh = overhang) {
    top <- pd[which((pd$term == x[2] | pd$term == x[3]) & !is.na(pd$estimate)), "ymax"] %>% max()
    bottom <- pd[which((pd$term == x[2] | pd$term == x[3]) & !is.na(pd$estimate)), "ymin"] %>% min()
    shift <- min(max(abs(top - round(top)), abs(round(bottom) - bottom)) + oh, .45)
    top <- round(top) + shift
    bottom <- round(bottom) - shift
    
    annotation_custom(grob = grid::linesGrob(),
                      xmin = farout + 0.5, xmax = farout + 0.5,
                      ymin = bottom, ymax = top)
  }
  
  draw_bracket_top <- function(x, oh = overhang) {
    top <- pd[which((pd$term == x[2] | pd$term == x[3]) & !is.na(pd$estimate)), "ymax"] %>% max()
    bottom <- pd[which((pd$term == x[2] | pd$term == x[3]) & !is.na(pd$estimate)), "ymin"] %>% min()
    shift <- min(max(abs(top - round(top)), abs(round(bottom) - bottom)) + oh, .45)
    top <- round(top) + shift
    bottom <- round(bottom) - shift
    
    annotation_custom(grob = grid::linesGrob(),
                      xmin = farout + 0.5, farout + 1,
                      ymin = top, ymax = top)
  }
  
  draw_bracket_bottom <- function(x, oh = overhang) {
    top <- pd[which((pd$term == x[2] | pd$term == x[3]) & !is.na(pd$estimate)), "ymax"] %>% max()
    bottom <- pd[which((pd$term == x[2] | pd$term == x[3]) & !is.na(pd$estimate)), "ymin"] %>% min()
    shift <- min(max(abs(top - round(top)), abs(round(bottom) - bottom)) + oh, .45)
    top <- round(top) + shift
    bottom <- round(bottom) - shift
    
    annotation_custom(grob = grid::linesGrob(), xmin = farout + 0.5, xmax = farout + 1,
                      ymin = bottom, ymax = bottom)
  }
  
  p2 <- p1 +
    theme_bw() +
    theme(plot.title = element_text(colour = NA),
          axis.title.y = element_blank(),
          axis.title.x = element_text(colour = NA),
          axis.text.y = element_blank(),
          axis.text.x = element_text(colour = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = NA),
          panel.border = element_rect(colour = NA),
          strip.background = element_rect(colour = NA, fill = NA),
          strip.text.x = element_text(colour = NA),
          plot.margin = unit(c(1,0,2,0), "lines"),
          legend.background = element_rect(colour = NA),
          legend.key = element_rect(colour = NA),
          legend.text = element_text(colour = NA)) +
    coord_cartesian(xlim = c(farout - 1, farout + 1)) +
    theme(legend.position = "none")
  
  for (i in seq(length(brackets))) {
    p2 <- p2 +
      draw_bracket_label(brackets[[i]]) +
      draw_bracket_vert(brackets[[i]]) +
      draw_bracket_top(brackets[[i]]) +
      draw_bracket_bottom(brackets[[i]])
  }
  
  plots <- list(p2, p1)
  grobs <- lapply(plots, function(x) ggplotGrob(x))
  max_heights <- list(do.call(grid::unit.pmax, lapply(grobs, function(x) x$heights)))
  grobs[[1]]$heights <- max_heights[[1]]
  grobs[[2]]$heights <- max_heights[[1]]
  
  pp <- ggplot(data.frame(x = 0:1, y = 0:1), aes_string(x = "x", y = "y")) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void() +
    labs(x = NULL, y = NULL) +
    draw_grob(grobs[[1]], 0, 1/9) +
    draw_grob(grobs[[2]], 1/9, 8/9)
  
  return(pp)
}

draw_grob <- function(grob, x, width) {
  layer(data = data.frame(x = NA),
        stat = StatIdentity,
        position = PositionIdentity,
        geom = GeomCustomAnn,
        inherit.aes = FALSE,
        params = list(grob = grob,
                      xmin = x,
                      xmax = width,
                      ymin = 0,
                      ymax = 1))
}