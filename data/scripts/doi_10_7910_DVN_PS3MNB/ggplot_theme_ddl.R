library(ggplot2)
library(reshape2)
library(dplyr)
library(grid)
library(scales)
library(lubridate)
library(extrafont)

# rm(list=ls())

font_import('~/GitHubRepositories/viz/corpID/fonts/', prompt = F)
fonts()


###
# to do:
## add fonts 
## title
## subtitle
## theme for title and subtitle
## ggsave with good dimensions
# transparent background
# replace static white in theme with transparent where it makes sense
# spacing

colourList <- 
  list(
    colour = list(
      gradient = c(
        "#dd2461",
        "#32BEAE"
      ),
      gradient2 = c(
        "#dd2461",
        "#ffffff",
        "#32BEAE"
      ),
      alt_gradient =c(
        '#5BC1F2', #lightblue
        "#dd2461"
        #"#3432AF" #dark blue
      ),
      n2 = c(
        "#32BEAE",
        "#dd2461"
      ),
      n12 = c(
        "#dd2461", 
        "#e2416d", 
        "#ea6884", 
        "#f394a4", 
        "#f7b5be", 
        "#f8c5cc",
        "#f8d3d8", 
        "#f8dfe2",
        "#f8e9eb", 
        "#FBF2F5", 
        "#FEF9FB", 
        "#ffffff"
      ),
      triplets = c(
        "#dd2461",
        "#e6e7e8",
        "#32BEAE"
      ),
      diffs = c(
        "#FB4250",
        "#D0D8D7",
        "#32BEAE"
      ),
      parties = list(
        # official srf colors: https://medium.com/srf-schweizer-radio-und-fernsehen/wie-wir-bei-srf-parteien-einf%C3%A4rben-9f010f80cf62
        svp = "#70e545",
        glp = "#C4C43D",
        grüne = "#bde545",
        gruene = "#bde545",
        gps = "#bde545",
        cvp = "#e59845",
        sp = "#e54a45",
        sps = "#e54a45",
        fdp = "#4590e5",
        bdp = "#ebd005",
        rest = "#d0d8d7",
        not = '#000000',
        # jungparteien
        jsvp = "#70e545",
        jglp = "#C4C43D",
        jgruene = "#bde545",
        jgrüne = "#bde545",
        jgps = "#bde545",
        jcvp = "#e59845",
        juso = "#e54a45",
        jf = "#4590e5",
        jbdp = "#ebd005"
      )
    ),
    grey = list(
      n12 = c(
        "#ffffff",
        "#ffffff", 
        "#ffffff", 
        "#ffffff", 
        "#898989", 
        "#7d7d7c", 
        "#717171", 
        "#656565", 
        "#595959", 
        "#4e4e4e", 
        "#414141", 
        "#343434"
      )
    )
  )

scaleHandler <- function(scale){
  scale <- ifelse(scale=='gray', 'grey', scale)
  scale <- ifelse(scale=='color', 'colour', scale)
  return(scale)
}

ddl_scale_colour_discrete <- function(scale, n, invert = F, ...){
  cols <- colourList[[scaleHandler(scale)]][[paste0('n', n)]]
  if(invert) cols <- rev(cols)
  scale_colour_manual(values = cols, ...)
}

ddl_scale_fill_discrete <- function(scale, n=NULL, name=NULL, invert = F, ...){
  if(!is.null(n)) cols <- colourList[[scaleHandler(scale)]][[paste0('n', n)]]
  if(!is.null(name)) cols <- colourList[[scaleHandler(scale)]][[name]]
  if(invert) cols <- rev(cols)
  scale_fill_manual(values = cols, ...)
}

ddl_scale_fill_continuous <- function(scale, .mid=NULL, ...){
  argList <- list(...)
  argNames <- ifelse(is.null(names(argList)), '', names(argList))
  print(.mid)
  if(!is.null(.mid)){
    scale_fill_gradient2(..., 
                         midpoint=.mid,
                         low = colourList[[scaleHandler(scale)]][['gradient2']][1],
                         mid = colourList[[scaleHandler(scale)]][['gradient2']][2],
                         high = colourList[[scaleHandler(scale)]][['gradient2']][3], 
                         na.value = 'lightgrey'
    )
  }else{
    # changed to alt_gradient on 23.09, because colorblindness, for pink-green use 'gradient'
    scale_fill_gradient(..., 
                        low = colourList[[scaleHandler(scale)]][['alt_gradient']][1],
                        high = colourList[[scaleHandler(scale)]][['alt_gradient']][2],
                        #low = colourList[[scaleHandler(scale)]][['gradient']][1],
                        #high = colourList[[scaleHandler(scale)]][['gradient']][2],
                        na.value = 'lightgrey'
    )
  }
}

ddl_scale_date <- function(.expand=c(.2,.2), ...){
  scale_x_date(
    labels = date_format("%m-%y"),
    expand = .expand,
    ...
  )
}

ddl_scale_continuous <- function(...){
  scale_y_continuous(
    expand = c(0.2, 0.2),
    ...
  )
}

rescale_colors <- function(cutoff, ...){
  function(x, to = c(0, 1), from = NULL){
    ifelse(x<cutoff, scales::rescale(x, to = to, from = c(min(x, na.rm = TRUE), cutoff)), 1)
  }
}

small_multiples <- function(.fill, ...){
  geom_rect(aes(fill = .fill), xmin = -Inf,xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 1)
}

ddl_theme <- function(..., 
                      type = 'default', 
                      titleSpacing = 5, 
                      subtitleSpacing = 20,
                      plotMargins = c(2,2,2,2), 
                      xTextAngle = 45,
                      backgroundFill='transparent',
                      panelFill='transparent',
                      baseFontsize = 14,
                      fontsizeTitle = baseFontsize*1.35,
                      fontsizeSubtitle = baseFontsize*1,
                      fontsizeAxisLabels = baseFontsize*.8,
                      fontsizeAxisTitles = baseFontsize*.9,
                      fontsizeLegendText = fontsizeAxisTitles,
                      fontsizeLegendTitle = fontsizeSubtitle,
                      fontsizeStrip = baseFontsize*.8,
                      vFlushTitle = NULL,
                      hFlushTitle = NULL,
                      vFlushSubtitle = NULL,
                      hFlushSubtitle = NULL,
                      stripFill = 'transparent',
                      stripTextCol = '#000000'){
  adjustValue <- switch((xTextAngle == 45) + 1, NULL, 1)
  if(type == 'default'){
    theme(
      plot.margin = unit(plotMargins, 'cm'),
      plot.background = element_rect(fill = backgroundFill, color = NA),
      panel.border = element_blank(),
      panel.background = element_rect(fill = panelFill, color = NA),
      strip.background = element_blank(),
      axis.title = element_text(family = 'Montserrat Bold', size = fontsizeAxisTitles),
      axis.text.y = element_text(family = 'Roboto', size = fontsizeAxisLabels),
      axis.text.x = element_text(angle = xTextAngle, hjust = adjustValue, family = 'Roboto', size = fontsizeAxisLabels),
      axis.title.x = element_text(vjust = -2),
      axis.title.y = element_text(vjust = 4),
      plot.title = element_text(family = 'Montserrat Bold', margin=margin(20,0,titleSpacing,0), size = fontsizeTitle),
      plot.subtitle = element_text(family = 'Roboto',  margin=margin(0,0,subtitleSpacing,0), size = fontsizeSubtitle),
      strip.text = element_text(family = 'Montserrat Bold', size = fontsizeStrip),
      legend.text = element_text(family = 'Roboto', size = fontsizeLegendText),
      legend.title = element_text(family = 'Montserrat Bold', size = fontsizeLegendTitle),
      ...
    )
  } else if(type == 'map'){
    theme(
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.background = element_rect(fill = panelFill, color = NA),
      plot.background = element_rect(fill = backgroundFill, color = NA),
      plot.title = element_text(family = 'Montserrat Bold', margin=margin(20,0,titleSpacing,0), size = fontsizeTitle),
      plot.subtitle = element_text(family = 'Roboto',  margin=margin(0,0,subtitleSpacing,0), size = fontsizeSubtitle),
      plot.margin = unit(plotMargins, 'cm'),
      strip.background = element_rect(fill = stripFill),
      strip.text = element_text(family = 'Montserrat Bold', colour = stripTextCol, size = fontsizeStrip),
      legend.text = element_text(family = 'Roboto', size = fontsizeLegendText, angle = 45, hjust=1),
      legend.title = element_text(family = 'Montserrat Bold', size = fontsizeLegendTitle),
      ...
    )
  } else if(type == 'no_grid'){
    theme(
      plot.margin = unit(plotMargins, 'cm'),
      plot.background = element_rect(fill = backgroundFill, color = NA),
      panel.border = element_blank(),
      panel.background = element_rect(fill = panelFill, color = NA),
      axis.title = element_text(family = 'Montserrat Bold', size = fontsizeAxisTitles),
      axis.text.y = element_text(family = 'Roboto', size = fontsizeAxisLabels),
      axis.text.x = element_text(angle = xTextAngle, hjust = adjustValue, family = 'Roboto', size = fontsizeAxisLabels),
      axis.title.x = element_text(vjust = -2),
      axis.title.y = element_text(vjust = 4),
      plot.title = element_text(family = 'Montserrat Bold', margin=margin(20,0,titleSpacing,0), size = fontsizeTitle, vjust = vFlushTitle, hjust = hFlushTitle),
      plot.subtitle = element_text(family = 'Roboto',  margin=margin(0,0,subtitleSpacing,0), size = fontsizeSubtitle, vjust = vFlushSubtitle, hjust = hFlushSubtitle),
      strip.background = element_rect(fill = stripFill),
      strip.text = element_text(family = 'Montserrat Bold', size = fontsizeStrip, colour = stripTextCol),
      legend.text = element_text(family = 'Roboto', size = fontsizeLegendText),
      legend.title = element_text(family = 'Montserrat Bold', size = fontsizeLegendTitle),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      ...
    )
  } else if(type == 'bar_withbg'){
    theme(
      panel.border=element_blank(),
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank(),
      panel.background = element_rect(fill = panelFill, color = NA),
      plot.background = element_rect(fill = backgroundFill, color = NA),
      plot.title = element_text(family = 'Montserrat SemiBold', margin=margin(20,0,titleSpacing,0), size = fontsizeTitle),
      plot.subtitle = element_text(family = 'Roboto',  margin=margin(0,0,subtitleSpacing,0), size = fontsizeSubtitle),
      plot.margin = unit(plotMargins, 'cm'),
      strip.background = element_rect(fill = '#000000'),
      strip.text = element_text(family = 'Montserrat SemiBold', colour = '#FFFFFF', size = fontsizeStrip),
      legend.text = element_text(family = 'Roboto', size = fontsizeLegendText),
      legend.title = element_text(family = 'Montserrat SemiBold', size = fontsizeLegendTitle),
      ...
    )
  }
}

GGsave <- function(format = 'rect', documentClass = 'paper', ...){
  
  if(documentClass == 'paper') .width <- 10
  else if(documentClass == 'poster') .width <- 15 
  
  argWidth <- switch(any(grepl('width',  names(list(...)))) + 1, list(width = unit(.width, 'cm')),  NULL)
  if(is.null(argWidth)){
    if(format == 'rect') argHeight <- switch(any(grepl('height',  names(list(...)))) + 1, list(height = list(...)$width - (list(...)$width/3)), NULL)
    else if(format == 'quadr') argHeight <- switch(any(grepl('height',  names(list(...)))) + 1, list(height = list(...)$width), NULL)
    message(paste('Dimensions:', list(...)$width, 'x', ifelse(is.null(argHeight), list(...)$height, argHeight), 'cm', collapse = ' '))
  }else{
    if(format == 'rect') argHeight <- switch(any(grepl('height',  names(list(...)))) + 1, list(height = .width - (.width/3)), NULL)
    else if(format == 'quadr') argHeight <- switch(any(grepl('height',  names(list(...)))) + 1, list(height = .width), NULL)
    message(paste('Dimensions:', .width, 'x', ifelse(is.null(argHeight), list(...)$height, argHeight), 'cm', collapse = ' '))
  }
  if(is.null(argWidth) & is.null(argHeight)) ggsave(...)
  else if(!is.null(argWidth) & is.null(argHeight)) ggsave(..., width = unit(argWidth, 'cm'))
  else if(is.null(argWidth) & !is.null(argHeight)) ggsave(..., height = unit(argHeight, 'cm'))
  else if(!is.null(argWidth) & !is.null(argHeight)) ggsave(..., width = unit(argWidth, 'cm'), height = unit(argHeight, 'cm'))
}

#### function to reverse datetime axis
#### (https://stackoverflow.com/questions/43625341/reverse-datetime-posixct-data-axis-in-ggplot/43626186)
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  name <- paste(a$name, b$name, sep = "-")
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
  
}

rev_date <- c_trans("reverse", "time")

# crop legend from plot
g_legend<- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

##### multilingual plots
GGlanguage <- function(keys, keychain = NULL, plotCall, type){
  for(m in c('de', 'fr', 'en')){
    if(is.null(keychain)) keychain <- names(keys[[m]])
    if(type == 'grid'){
      
      # title
      .title <- textGrob(keys[[m]][[keychain[1]]], 
                         x=0.02, # indent
                         hjust=0, 
                         gp=gpar(fontsize=13, fontfamily='Montserrat Bold'))
      # subtitle grob
      .subtitle <- textGrob(keys[[m]][[keychain[2]]], 
                            x=0.02, # indent
                            y=0.65, hjust=0, 
                            gp=gpar(fontsize=10, fontfamily='Open Sans'))
      
    }else if(type == 'labs'){
      
      .title <- keys[[m]][[keychain[1]]]
      .subtitle <- keys[[m]][[keychain[2]]]
      
    }
    # print(Subtitle)
    plotCall(.title, .subtitle, i=m)
  }
}
