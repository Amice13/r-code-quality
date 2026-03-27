# source of split violin plot - https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2 

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

#### same size labels box #### https://stackoverflow.com/questions/48195587/how-to-set-a-standard-label-size-in-ggplots-geom-label

ggname <- function (prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

geom_label2 <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        parse = FALSE,
                        nudge_x = 0,
                        nudge_y = 0,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines"),
                        label.size = 0.25,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabel2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...
    )
  )
}

GeomLabel2 <- ggproto("GeomLabel2", Geom,
                      required_aes = c("x", "y", "label"),
                      
                      default_aes = aes(
                        colour = "black", fill = "white", size = 3.88, angle = 0,
                        hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                        lineheight = 1.2
                      ),
                      
                      draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                            na.rm = FALSE,
                                            label.padding = unit(0.25, "lines"),
                                            label.r = unit(0.15, "lines"),
                                            label.size = 0.25) {
                        lab <- data$label
                        if (parse) {
                          lab <- parse(text = as.character(lab))
                        }
                        
                        data <- coord$transform(data, panel_params)
                        if (is.character(data$vjust)) {
                          data$vjust <- compute_just(data$vjust, data$y)
                        }
                        if (is.character(data$hjust)) {
                          data$hjust <- compute_just(data$hjust, data$x)
                        }
                        
                        grobs <- lapply(1:nrow(data), function(i) {
                          row <- data[i, , drop = FALSE]
                          labelGrob2(lab[i],
                                     x = unit(row$x, "native"),
                                     y = unit(row$y, "native"),
                                     just = "center",
                                     padding = label.padding,
                                     r = label.r,
                                     text.gp = gpar(
                                       col = row$colour,
                                       fontsize = row$size * .pt,
                                       fontfamily = row$family,
                                       fontface = row$fontface,
                                       lineheight = row$lineheight
                                     ),
                                     rect.gp = gpar(
                                       col = row$colour,
                                       fill = alpha(row$fill, row$alpha),
                                       lwd = label.size * .pt
                                     )
                          )
                        })
                        class(grobs) <- "gList"
                        
                        ggname("geom_label", grobTree(children = grobs))
                      },
                      
                      draw_key = draw_key_label
)

labelGrob2 <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                       just = "center", padding = unit(0.25, "lines"), r = unit(0.1, "snpc"),
                       default.units = "npc", name = NULL,
                       text.gp = gpar(), rect.gp = gpar(fill = "white"), vp = NULL) {
  
  stopifnot(length(label) == 1)
  
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  
  gTree(label = label, x = x, y = y, just = just, padding = padding, r = r,
        name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "labelgrob2")
}

makeContent.labelgrob2 <- function(x) {
  hj <- resolveHJust(x$just, NULL)
  vj <- resolveVJust(x$just, NULL)
  
  t <- textGrob(
    x$label,
    x$x + 1 * (0.55 - hj) * unit(5, "mm"),
    x$y + 2 * (0.55 - vj) * x$padding,
    just = "center",
    gp = x$text.gp,
    name = "text"
  )
  
  r <- roundrectGrob(x$x, x$y, default.units = "native",
                     width =  0.4 * unit(max(stri_width(x$x)) + 1, "mm"),
                     height = grobHeight(t) + 2 * x$padding,
                     just = c(hj, vj),
                     r = x$r,
                     gp = x$rect.gp,
                     name = "box"
  )
  
  setChildren(x, gList(r, t))
}

# load fonts

# Load fonts - 
# font_import() 

fonts()
loadfonts(quiet = T)
loadfonts(device = "win")

# custom functions

'%!in%' <- function(x,y)!('%in%'(x,y)) # from https://stackoverflow.com/questions/5831794/opposite-of-in

# string to exclude experts below 5 years of experience
young <- c("0 - 4")

# Scenario color codes

cols_new <- c("1: Economic convergence\n    and unilateralism" = "#418FDE",
              "2: Economic convergence\n    and multilateralism" = "#5CB8B2",
              "3: Economic divergence\n    and unilateralism" = "#FFB81C",
              "4: Economic divergence\n    and multilateralism" = "#D22630")

# Scales

scaleFUN_million <- function(x) sprintf("%.f", x/1000000)
scaleFUN <- function(x) sprintf("%.1f", x/1000000)
scaleFUN_th <- function(x) sprintf("%.f", x/1000)

# disable scientific notation

options(scipen=999)


