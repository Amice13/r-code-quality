##################################################################################################
############# Spatial Visualization and Analysis for the Social Sciences Using R #################
##################################################################################################

# Copyright (C) 2012 - Rodrigo Rodrigues-Silveira
# Research Fellow - Ibero-American Institute - Salamanca University
# Post-Doc Fellow - Latein-Amerika Institut - Freie Universität Berlin

# rodrodr@gmail.com

# Last revision: 01-Jul-2012

# This code is distributed under the terms of the GNU General Public License, either Version 2, June 1991 or Version 3, June 2007. Copies of both versions 2 and 3 of the license can be found at http://www.gnu.org/licenses/gpl.html.

##################################################################################################
##################################################################################################
##################################################################################################


library(compiler)

##### Function colmap() #####

# colmap() - establishes the color divisions, palettes and breaks to maps and other graphical representations. It uses basically RColorBrewer to provide color schemes, but user-defined palettes can be passed to the function by the argument custcol=.

# Parameters:
#             val - variable to be use to classify cases
#             classint - classification method to be used. The possible values are: "unique", "user", 
#                         "unclassed", "sd","quantile","equal","kmeans","jenks"
#             palet - the RColorBrewer palette to be used in the color classification
#             userbrk - the user defined break points in the variable when classint="user".
#             nbrks - number of breaks to divide cases
#             invpalet - logical - determines whether to invert the order of colors in the palet or 
#                       not. This is specially usefull when some palets start with darker colors, as
#                       Greys, or are ordered in a contra-intuitive order, as in some divergent palets
#                       like "RdYlBu", "RdYlGn", "RdGn","RdBu","PuOr","PRGn" or "BrBG".
#             custcol - list of custom colors to be used in the classification.



colmap <- function(val, classint="quantile", palette="Blues", userbrk=NULL, nbrks=5, invpalet=F,custcol=NULL, transp=99, na.col=NULL){
  
  require(RColorBrewer)
  require(classInt)
  
  if(is.null(na.col)) val[is.na(val)] <- 0

        
  if (! is.null(custcol)){ 
    palette <- custcol
  }else{  
    pal <- palette
    palette <- brewer.pal(9, palette)
    if (pal=="Greys") palette <- palette[1:length(palette)-1]
    if (invpalet==T) palette <- palette[length(palette):1]
  }
      
  col <- colorRampPalette(palette)

  if (classint=="unclassed"){
    groups <- 0
    colors <- col(length(val))
    palette <- colors
    colors <- colors[findInterval(val,sort(val), all.inside=T)]
    pct.cases <- 0
  }
  

  if (classint=="unique"){
groups
    groups <- sort(unique(val))

    if(is.character(val)) val <- as.factor(val)
    if(is.factor(val)) val <- as.numeric(val)
    if (length(groups)>length(custcol)) colors <- col(length(groups)) else colors <- custcol
    palette <- unique(colors)
    colors <- colors[findInterval(val,sort(as.numeric(unique(val))), all.inside=F)]
    pct.cases <- table(colors)[order(palette)]/length(colors)*100
  }

  
  
  if (classint=="user"){
    if (is.null(userbrk)) stop("El mapa de tipo 'User' requiere que los intervalos sean providos por el usuario!")

    groups <- userbrk
    colors <- col(length(userbrk)-1)
    palette <- unique(colors)
    colors <- colors[findInterval(val,userbrk, all.inside=T)] 
    pct.cases <- table(colors)[order(palette)]/length(colors)*100
    
  }

  if (classint %in% c("sd","quantile","equal","kmeans","jenks","pretty")){
      
    if (classint=="jenks"){
      if (length(val)>900){groups <- classIntervals(var=sample(val,120), n=nbrks, style=classint)
      }else {groups <- classIntervals(var=val, n=nbrks, style=classint)}
      }       
    if (classint!="jenks") groups <- classIntervals(var=val, n=nbrks, style=classint)
    
    groups <- unique(groups[[2]])
    colors <- col(length(groups)-1)
    palette <- unique(colors)
    colors <- colors[findInterval(val,groups, all.inside=T)] 
    pct.cases <- table(colors)[order(palette)]/length(colors)*100
    
  }
  
  if(length(colors[is.na(colors)])>0) colors[is.na(colors)] <- na.col
  
  
  if (transp!=99){
  colors <- paste0(colors, transp, sep="")
  palette <- paste0(palette, transp)}
  
  return(list(colors=colors, groups=groups, palette=palette, pct.cases=pct.cases))

}  



compile(colmap)


adleg <- function(legpos="bottomleft",leg.main="", cex.leg=1.4, pt.cex=0.8, y.intersp=0.8, x.intersp=0.5, pch=19, classint="quantile", val=NULL, palette=NULL,userbrk=NULL,groups=NULL,dec=1, plot.pct=FALSE, pct.cases=NULL, cl="SpatialPolygonsDataFrame",   big.mark=".", decimal.mark=",",horiz=F, add=T, symbol.bg="red", symbol.fg="black", leg.col="black", na.col=NULL, na.lab="n.a.", leg.bord="transparent"){
  
  require(maptools)
  
  par(mar=rep(0,4))

  if (is.numeric(legpos)) legpos <- cbind(legpos[1], legpos[2])
                                              
  if (classint=="unclassed"){
    
    # Open the library fields required to the progressive color bar
    require(fields)
    limites <- c(min(val,na.rm=T),max(val, na.rm=T))
    image.plot(legend.only=T, zlim=limites, col= sort(unique(palette),decreasing=T), smallplot=c(0.05,0.08,0.05,0.5))
  }  
  
  
  if (classint!="unclassed"){
    
    if (classint=="user"& is.null(userbrk)) stop("O mapa de tipo 'user' exige que os intervalos sejam fornecidos pelo usuário!")
    
    if (classint=="unique"){
      if (is.factor(val)) legs <- levels(val) else legs <- as.character(groups)}
    
    if (classint !="unique") legs <- leglabs(format(round(groups, dec), big.mark=big.mark, decimal.mark=decimal.mark), under="<", over=">")

    if (plot.pct==TRUE)legs <- paste(legs,paste("(", format(round(pct.cases, dec), big.mark=big.mark, decimal.mark=decimal.mark), "%)", sep=""), sep=" ")
    
    if(add==F) plot(1, col="transparent", axes=F)

    if (! is.null(na.col)) {
      legs <-  c(na.lab,legs)
      palette <- c(na.col, palette) 
    }
  
      if (cl%in% c("SpatialPolygons","SpatialPolygonsDataFrame")){
      legend(legpos,legend=legs,fill=palette, bg="transparent", bty="n", cex=cex.leg, y.intersp=y.intersp, x.intersp=x.intersp, title=leg.main, title.adj=0.15, horiz = horiz, text.col = leg.col, border = leg.bord)}
    else if (cl%in% c("SpatialPoints","SpatialPointsDataFrame")){
      legend(legpos,legend=legs, pt.bg = symbol.bg, border=symbol.fg, bg="transparent", bty="n", pch=pch, pt.cex=pt.cex, cex=cex.leg, y.intersp=y.intersp, x.intersp=x.intersp, title=leg.main, title.adj=0.15, horiz = horiz, text.col = leg.col, col=palette)
    }
    
  }
  
}

##### Function colBright() #####

# Determines if the bightness of the color is too dark and adapts the label colour to get the best adjustment for visibility
# Parameters:
#            colors - vector with color names or hexadecimal codes to be tested
#            lim    - limit in terms of luminosity to cut from dark to bright color
#            bright - color of the text in dark background colors. The default is white "#ffffff"
#            dark - color of the text in bright backround colors. The default is black "#000000"
colBright <- function(colors, lim=130, bright="#ffffff", dark="#000000"){
  
  require(grDevices)
  
  rgb <- col2rgb(colors)
  
  for(i in 1:ncol(rgb)){
    val <- 0.2126*rgb[1,i]+0.7152*rgb[2,i]+0.0722*rgb[3,i]
    if(i==1) valc <- val else valc <- c(valc,val)
  }
  
  col <- rep(bright, length(colors))
  col[valc>lim] <- dark
  
  col
  
}



##### Function adlab() #####

### Adds labels to a map
### This function is designed to add labels or description to spatial objects on a map, be them points, lines or areas (polygons).
## Parameters:
#         spobj - spatial object used to define the location of the labels
#         var   - character vector with a list of labels for each spatial unit on the map (it must have the same length of spobj)
#         cex   - size of the text to be represented. The default value is 1.
#         font  - the True Type Font employed to represent labels. The default is "Times New Roman".
#         text.col   - the color of the text. The default is "black". 
#         pos   - the same description as in the text function: "a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates".
#         col.bright - changes the color of the text according to the brightness of the background colors.
#         lab.font - the style of the text: 1 normal, 2 bold, 3 italic, 4 bold-italic.
adlab <- function(spobj, labels, cex=1, font="Times New Roman", text.col="black", pos=NULL, col.bright=NULL, lab.font=1){
  
  require(sp)
  
  windowsFonts(times=windowsFont(font))
  par(family="times")
  
  if (! is.null(col.bright)){
    
    text.col <- colBright(col.bright)
    
  }
  
  if (! class(spobj) %in% c("SpatialPoints","SpatialPointsDataFrame")){
    text(coordinates(spobj), labels=labels, pos = pos, cex = cex, col = text.col, adj = 0.5, offset = 0,font=lab.font)
  }else{ 
    text(spobj, labels=labels, pos = pos, cex = cex, col = text.col, adj = 0.5,offset = 0,font=lab.font)
  }

}


##### Function mapa() #####

# mapa() - is the basic map function. Its purpose is to provide easy ways to classify and generate thematic maps, using point, area, and line spatial objects. 

# Parameters:
#             spobj - variable to be use to classify cases
#             val - 
#             classint - classification method to be used. The possible values are: "unique", "user", 
#                         "unclassed", "sd","quantile","equal","kmeans","jenks"
#             nbrks - number of breaks to divide cases
#             userbrk - the user defined break points in the variable when classint="user".
#             title - title of the map.
#             palet - the RColorBrewer palette to be used in the color classification
#             invpalet - logical - determines whether to invert the order of colors in the palet or 
#                       not. This is specially usefull when some palets start with darker colors, as
#                       Greys, or are ordered in a contra-intuitive order, as in some divergent palets
#                       like "RdYlBu", "RdYlGn", "RdGn","RdBu","PuOr","PRGn" or "BrBG".
#             legpos - legend position.
#             dec - decimals to be used in the legend
#             cex.leg - character expantion factor for the legend
#             y.intersp - vertical interval space between classes in the legend
#             smallplot - coordinates to draw the color bar used in the "unclassed" maps
#             border - border color
#             lwd0 - line width for the base map (first layer)
#             frame1 - layer map used to me drawn over the base map
#             bordframe1 - border color of frame1
#             lwd1 - line width of borders of frame1
#             frame2 - layer map used to me drawn over the base map and the frame1
#             bordframe2 - border color of frame2
#             lwd2 - line width of borders of frame2
#             frame3 - layer map used to me drawn over the base map, frame1, and frame2
#             bordframe3 - border color of frame3
#             lwd3 - line width of borders of frame3
#             pch -  point character to be used in a dot density map
#             pt.cex - point character expantion factor of the dot density map
#             leg.main - title of the map legend
#             pct.cases - logical - displays the percentage of cases in each category on the map
#             custcol - custom colors defined by the user (in the case that it is not desirable 
#                       to use one of the RColorBrewer palettes)
#             ... - other default parameters given directly to the plot() function.


mapa <- function(spobj=NULL, val=NULL, classint="quantile", nbrks=5, userbrk=NULL, title=NULL, palette="Blues", invpalet=F, legpos="bottomleft", dec=1,  cex.leg=1.4, y.intersp=0.5, x.intersp=0.5, smallplot=c(0.05,0.08,0.05,0.5),  border="black", lwd0=1, frame1=NULL, bordframe1="black",lwd1=1,frame2=NULL, bordframe2="black", lwd2=2, frame3=NULL, bordframe3="black", lwd3=2,pch=19, pt.cex=0.8, leg.main="", pct.cases=FALSE, custcol=NULL, transp=99, font="Times New Roman",leg.return=F, leg.draw=T, leg.horiz=F, big.mark=".", decimal.mark=",", labels=NULL,lab.cex=1, lab.col="black", lab.pos=NULL, lab.font=1, col.bright=NULL, add=F, leg.col="black",leg.bord="transparent",na.col=NULL,na.lab="n.a.",xlim=NULL, ylim=NULL,...){
  
  windowsFonts(times=windowsFont(font))
  par(family="times")
  
  #   # Parte I - configuracao basica e checagem geral
  if (is.null(spobj) | is.null(val)) stop("Un valor válido debe ser suministrado para los parámetros mapobj (objeto espacial) y val (variable que se desea representar en el mapa)!")
  #   
  if(is.null(title)) par(mar=rep(0.1,4)) else par(mar=c(0.1,0.1,2,0.1))
  # if(! is.factor(val)) val[is.na(val)] <- 0

  require(maptools)
  
  if(class(val) %in% c("character","factor")){
    classint <- "unique"
    if (palette=="Blues") palette <- "Set3"}  
  
  
  
  # Parte II - obtem grupos e cores  
  mapres <- colmap(val = val, classint=classint, nbrks=nbrks, palette=palette, userbrk=userbrk, invpalet=invpalet, custcol=custcol, transp = transp, na.col = na.col)
  colores <- mapres[[1]]
  grupos <- mapres[[2]]
  paleta <- mapres[[3]] 
  pct.casos <- mapres[[4]]
  
  
  
    if (! is.null(frame1)){
      xlia <- frame1@bbox[1,]
      ylia <- frame1@bbox[2,]
    }else{
      xlia <- spobj@bbox[1,]
      ylia <- spobj@bbox[2,]
    }
  
    if(! is.null(xlim)) xlia <- xlim
    if(! is.null(ylim)) ylia <- ylim
  
  
  cl <- class(spobj)
  
  # Parte III - Desenha o mapa (e até três camadas superiores)
  if (cl %in% c("SpatialPolygons","SpatialPolygonsDataFrame")){
    plot(spobj, col=colores, border=border, lwd=lwd0, add=add,xlim=xlia, ylim=ylia)
  }
  else if (cl %in% c("SpatialPoints","SpatialPointsDataFrame")){
    plot(spobj, col=colores,cex=pt.cex,pch=pch, add=add,xlim=xlia, ylim=ylia)
  }

  if (! is.null(frame1)) plot(frame1, border=bordframe1, add=T, lwd=lwd1,xlim=xlia, ylim=ylia)
  if (! is.null(frame2)) plot(frame2, border=bordframe2, add=T, lwd=lwd2,xlim=xlia, ylim=ylia)
  if (! is.null(frame3)) plot(frame3, border=bordframe3, add=T, lwd=lwd3,xlim=xlia, ylim=ylia)
    
  #   # Parte IV: Adiciona o t??tulo (se existente), legenda ou barra de cores (se "unclassed")
  if (! is.null(title)) title(main=title)
    
  
  if (! is.null(labels)) adlab(spobj=spobj, labels=labels, cex=lab.cex, font=font, text.col=lab.col, pos=lab.pos, col.bright=colores,lab.font = lab.font)
  
  if (leg.return==T) return(list(legpos=legpos,leg.main=leg.main, cex.leg=cex.leg, y.intersp=y.intersp,x.intersp=x.intersp, pch=pch, classint=classint, val=val, palette=paleta,userbrk=userbrk,groups=grupos,dec=dec, pct.cases=pct.cases, pct.casos=pct.casos, cl=cl, leg.col=leg.col))
  
  if (leg.draw==T) adleg(legpos = legpos,leg.main = leg.main, cex.leg = cex.leg, y.intersp = y.intersp,x.intersp = x.intersp, pch = pch, classint = classint, val = val, palette = paleta,userbrk = userbrk,groups = grupos,dec=dec, plot.pct = pct.cases, pct.cases = pct.casos, cl = cl, big.mark = big.mark, decimal.mark = decimal.mark, horiz=leg.horiz, leg.col=leg.col, na.col=na.col, na.lab=na.lab, leg.bord=leg.bord)
    
}

compile(mapa)



### LISA map ####

# Generates a LISA map of three different cluster methods: Local Moran's I, Local G and Ripleys' K.

# Parameters:
#             spobj - spatial object used (spatial polygon or spatial points).
#             val - variable used to calculate the local I and local G.
#             lwobj - contiguity matrix used to calculate the local I and local G (optional).
#             frame - spatial object used to plot the geographic context of clusters.
#             border - border color of the spatial polygon object. The default is "black".
#             type - type of spatial cluster map to be generated: 
#                     i - Moran's local I; 
#                     g - Getis and Ord's local G; 
#                     k - Ripley's K, for point patterns.
#             vardesc - description of the var (val) employed or the attribute from the spatial 
#                       point pattern.
#             rvalue - radius, in kilometers employed to compute the Ripley's local K value
#             cex - size of the points to be plotted in the map (only spatial points patterns)
#             lang - language of the legend (english -"en", spanish - "es", portuguese - "pt")
#             palet - four basic colors used to represent categories.
#             alpha - level of significance to be applied
#             title - logical - title of the map containing the spatial correlation coef.
#             categ - number of categories to be used (2 or 4)



lisamap <- function(spobj=NULL, val=NULL, val2=NULL, lwobj=NULL, frame=NULL, border="black", bordframe="black", lwd=1, type="i", vardesc=NULL, rvalue=50, cex=0.7, lang="en", palette=c("royalblue","red2","salmon","orchid1"), alpha=0.05, title=F, cex.leg=1, categ=2, leg.pos="bottomleft"){
  
  require(maptools)
  require(spdep)
  require(spatstat)
  
  val[is.na(val)] <- mean(val, na.rm=T)
  
  
  if(is.null(lwobj) & class(spobj) %in% c("SpatialPolygonsDataFrame", "SpatialPolygons")){
    lwobj <- poly2nb(spobj[! is.na(val),], queen=T)
    lwobj <- nb2listw(lwobj,zero.policy=T)
  }
  
  
  lcm <- localmoran(val, lwobj, zero.policy = T)
  
  
  # Z-value confidence interval
  zlim <- qnorm(1-alpha)
  zlima <- -1*zlim
  
  if (! is.null(val)){  
    val[is.na(val)] <- mean(val, na.rm=T)
    zx <- scale(val, center=T,scale=T)
    
    if (type %in% c("i","g")){
      if (is.null(val2)) lagzx <- scale(lag.listw(lwobj, val, zero.policy = T), center=T,scale=T)
      if (! is.null(val2)) {
        val2[! is.na(val2)] <- mean(val2, na.rm=T)
        lagzx <- scale(lag.listw(lwobj, val2, zero.policy = T), center=T,scale=T)}
    }
  }
  
  if (type=="i"){
    
    lmoran <- localmoran(val, listw=lwobj, zero.policy=T)
    mr <- moran.test(val, listw=lwobj, zero.policy=T)
    
    
    # reg <- c("royalblue","red2","mediumpurple1","plum1")
    
    # Extract both the Moran I for each observation and its significance level
    sig_moran <- round(as.vector(lmoran[,5]),3)
    
    if (categ==2){    
      if (lang=="en") desc <- c("Low-Low","High-High","Moran's I:")
      if (lang=="es") desc <- c("Bajo-Bajo","Alto-Alto","I de Moran:")
      if (lang=="pt") desc <- c("Baixo-Baixo","Alto-Alto","I de Moran:")
      # Assign a color to each category
      spobj$color <- "transparent"
      spobj$color[(zx <0 & lagzx<0) & sig_moran<=alpha] <- palette[1] # Baixo - Baixo
      spobj$color[(zx >0 & lagzx>0) & sig_moran<=alpha] <- palette[2] # Alto - Alto
    }else{
      if (lang=="en") desc <- c("Low-Low","High-High","Low-High","High-Low","Moran's I:")
      if (lang=="es") desc <- c("Bajo-Bajo","Alto-Alto","Bajo-Alto","Alto-Bajo","I de Moran:")
      if (lang=="pt") desc <- c("Baixo-Baixo","Alto-Alto","Baixo-Alto","Alto-Baixo", "I de Moran:")
      # Assign a color to each category
      spobj$color <- "transparent"
      spobj$color[(zx <0 & lagzx<0) & sig_moran<=alpha] <- palette[1] # Baixo - Baixo
      spobj$color[(zx >0 & lagzx>0) & sig_moran<=alpha] <- palette[2] # Alto - Alto
      spobj$color[(zx <0 & lagzx>0) & sig_moran<=alpha] <- palette[3] # Baixo - Alto
      spobj$color[(zx >0 & lagzx<0) & sig_moran<=alpha] <- palette[4] # Alto - Baixo      
    }
    
    
    #     spobj$color <- "transparent"
    #     spobj$color[zx <zlima & lagzx<zlima] <- palet[1] # Baixo - Baixo
    #     spobj$color[zx >zlim & lagzx>zlim] <- palet[2] # Alto - Alto
    #     spobj$color[zx <zlima & lagzx>zlim] <- palet[3] # Baixo - Alto
    #     spobj$color[zx >zlim & lagzx<zlima] <- palet[4] # Alto - Baixo
    
    leg <- desc[1:length(desc)-1]
    titlet <- paste(desc[3], round(mr$estimate[1],3), vardesc, sep=" ")  
    reg <- palette
    
    
  }
  
  if (type=="g"){

    lG <- localG(val, listw=lwobj, zero.policy=T)

    if (lang=="en") leg <- c("Low","High","Geary's G:")
    if (lang=="es") leg <- c("Bajo","Alto","G de Geary:")
    if (lang=="pt") leg <- c("Baixo","Alto","G de Geary:")
    
    titlet <- paste(leg[3], vardesc, sep=" ")
    
    leg <- leg[1:2] 
    reg <- palette[1:2]

    spobj$color <- "transparent"
    spobj$color[lG< zlima] <- palette[1] # Low values cluster
    spobj$color[lG> zlim] <- palette[2]   # High values cluster
    
  }
  
  # localC
  # if (type=="c"){
  #   
  #   geac <- localC(val, lwobj)
  # 
  #   if (lang=="en") leg <- c("Positive","Negative","Geary's C:")
  #   if (lang%in% c("es","pt")) leg <- c("Positivo","Negativo","C de Geary:")
  # 
  #   spobj$color <- "transparent"
  #   spobj$color[geac < 1] <- palette[2]   # Positive autocorrelation
  #   spobj$color[geac > 1] <- palette[1]   # Negative autocorrelation
  # 
  #   titlet <- leg[3]
  #   leg <- leg[1:2]
  #   reg <- palette[2:1]
  # }
  # 
  
  if (type=="k"){
    
    dist <- (1/111)*rvalue
    
    # Calculates the value of Ripley's local K for a radius of 250 meters
    if (!class(spobj)%in% c("SpatialPoints","SpatialPointsDataFrame")) spobj <- SpatialPointsDataFrame(coordinates(spobj),data.frame(spobj))
    zk <- scale(localK(as.ppp(spobj), rvalue=dist))
    
    
    # Select those schools that are particularly disperse (blue) or concentrated (red) (less than 1 SD and more than 1SD)
    spobj$color <- "transparent"
    spobj$color[zk < zlima] <- palette[1]
    spobj$color[zk > zlim] <- palette[2]
    
    
    if (lang=="en") leg <- c("Disperse","Concentrated","Ripley's K:")
    if (lang=="es") leg <- c("Disperso","Concentrado","K de Ripley:")
    if (lang=="pt") leg <- c("Disperso","Concentrado","K de Ripley:")
    
    titlet <- paste(leg[3], vardesc, "- radius:", rvalue, "km", sep=" ")
    reg <- palette[1:2]
    leg <- leg[1:2]
  }
  
  
  # Plot the results
  
  if (title==T){
    par(mar=c(0,0,2,0)) 
    main <- titlet}
  
  if (title==F) {
    par(mar=c(0,0,0,0)) 
    main <- ""}

  if(! is.null(frame)) bframe <- bbox(frame) else bframe <- bbox(spobj)
    
  if(type=="k"){plot(spobj, col=spobj$color, pch=19, cex=cex, main=main, xlim=bframe[1,], ylim=bframe[2,]) 
                }else{
                  plot(spobj, col=spobj$color, border=border, lwd=lwd, main=main, xlim=bframe[1,], ylim=bframe[2,])}

  if(! is.null(frame)) plot(frame, border=bordframe, lwd=lwd, add=T)
  
  if (title==T) title(main=main)
  
  legend(leg.pos, legend=leg, fill=reg, bty="n", cex=cex.leg)
  
}


### Quadrant map #####

# Creates the map of quadrants of a bi-dimensional plot (scatterplot).

# Parameters:
#             spobj - spatial object to be used as base to create the map
#             x - a continuous variable with the same length as spobj
#             y - a continuous variable different from x with the same length as spobj
#             frame - spatial object used to frame the areas from spobj (optional).
#             border - border color of spobj. The default is "black".
#             zlim - the lower threshold value in standard deviations (z-value) to include an 
#                   observation as a cluster. If, for example, zlim=1, it will be necessary that both #                   x and y have  z-value higher than 1 in order to be considered  high-high cluster.
#             lang - language of the legend. options: en - english, es - spanish, pt - portuguese.
#             palette - the color palette compose of four basic colors to be used in identifying each category. The default is "royalblue","red2","#FFFF99","#7FC97F".
#             lwd - line width of the frame layer
#             leg.pos - legend position
#             cor.pos - position of the correlation coeficient to be displayed in the map

qdmap <- function(spobj, x, y, frame=NULL, border="black", zlim=0, lang="en", cex.leg=1, palet=c("royalblue","red2","#FFFF99","#7FC97F"), lwd=1, lwd.frame=2, leg.pos="bottomleft", cor.pos= c(-41.71177, -31.07674),y.intersp=0.52, x.intersp=0.2, selqad=1, add=F){
  
  zx <- scale(x)
  zy <- scale(y)
  
  zlima <- -1*zlim
  
  if (selqad==2) palet[3:4] <- "transparent"
  if (selqad==3) palet[1:2] <- "transparent"
  
  
  spobj$color <- "transparent"
  spobj$color[zx<=zlima & zy<=zlima] <- palet[1]
  spobj$color[zx>zlim & zy>zlim] <- palet[2]
  spobj$color[zx<=zlima & zy>zlim] <- palet[3]
  spobj$color[zx>zlim & zy<=zlima] <- palet[4]
  
  spobj$count <- NA
  spobj$count[zx<=zlima & zy<=zlima] <- 1
  spobj$count[zx>zlim & zy>zlim] <- 2
  spobj$count[zx<=zlima & zy>zlim] <- 3
  spobj$count[zx>zlim & zy<=zlima] <- 4
  
#   tot <- length(spobj$count[! is.na(spobj$count)])   
  tot <- length(spobj$count[spobj$color!="transparent"])   
  pbb <- round(length(spobj$count[spobj$count==1])/tot*100,1)
  paa <- round(length(spobj$count[spobj$count==2])/tot*100,1)
  pba <- round(length(spobj$count[spobj$count==3])/tot*100,1)
  pab <- round(length(spobj$count[spobj$count==4])/tot*100,1)
  
  par(mar=c(0.1,0.1,0.1,0.1))
  plot(spobj, col=spobj$color, border=border, lwd=lwd, add=add)
  
  if (lang=="en") desc <- c("Low-Low","High-High","Low-High","High-Low","Quadrant: % of cases","Threshold=")
  if (lang=="es") desc <- c("Bajo-Bajo","Alto-Alto","Bajo-Alto","Alto-Bajo","Cuadrante: % de casos","Umbral=")
  if (lang=="pt") desc <- c("Baixo-Baixo","Alto-Alto","Baixo-Alto","Alto-Baixo", "Quadrante: % de casos","Limite=")

  if (selqad==1){
    leg <- c(paste0(desc[1],": ", pbb, "%"),paste0(desc[2],": ", paa, "%"),paste0(desc[3],": ", pba, "%"),paste0(desc[4],": ", pab, "%"))} 
  
  if (selqad==2){
    leg <- c(paste0(desc[1],": ", pbb, "%"),paste0(desc[2],": ", paa, "%"))
    palet <- palet[1:2]} 
  
  if (selqad==3){
    leg <- c(paste0(desc[3],": ", pba, "%"),paste0(desc[4],": ", pab, "%"))
    palet <- palet[3:4] }
  
  
  if(! is.null(frame)) plot(frame, add=T, lwd=lwd.frame)
  
  if (! is.numeric(leg.pos)) legend(leg.pos, legend=leg, fill=palet, bty="n", cex=cex.leg, title=desc[5], title.adj=0.15, y.intersp=y.intersp, x.intersp=x.intersp)
  
  if (is.numeric(leg.pos))legend(leg.pos[1], leg.pos[2], legend=leg, fill=palet, bty="n", cex=cex.leg, title=desc[5], title.adj=0.15, y.intersp=y.intersp, x.intersp=x.intersp)
  
  
  if (cex.leg>1.3) cex.r <- cex.leg else cex.r <- 1.3
  
  
  text(cor.pos[1], cor.pos[2], labels=paste("r=", round(cor( x, y,method="spearman", use="complete.obs"),3), sep=" "), cex=cex.r)
  
  #  if (zlim>0) text(cor.pos[1], (cor.pos[2]+cor.pos[2]*0.1), labels=paste(desc[6], zlim, "s.d.", sep=" "), cex=cex.r-0.3)
  
  
}




#### High-low map - multivariate cluster map ####

# Using a numeric matrix as base, this function constructs a map of clustered high and low values. It is useful to create a multivariate representation of complex fenomena when different dimensions, assumed to vary in the same direction are clustered and should be represented together (find examples).

# Parameters:
#             spobj - spatial object to be represented.
#             data - numeric data frame used in the calculations.
#             frame - spatial object used to frame the basic map.
#             border - border color of the basic map. The default is "black".
#             zlim - limit, in Z-value to color spatial units. The default is 0.
#             lang - language (english -"en", spanish - "es", portuguese - "pt"). The default is "en".
#             cex.leg - character expantion factor of the legend. The default is 1.
#             lwd.frame - width of the border of the frame object. The default is 2.
#             legpos - position of the legend in the map. The default is "bottomleft".
#             palette - color palette used to represent high and low values. The default is "red" for high values and "blue" for low ones.

hilomap <- function(spobj, data, frame=NULL, border="black", zlim=0, lang="en", cex.leg=1, lwd.frame=2, legpos="bottomleft", palette=c("red","blue"),...){
  
  x <- scale(data)
  
  mathigh <- x>zlim
  matlow  <- x<(-1*zlim)
  
  idhigh <- rowSums(mathigh)
  idlow <- rowSums(matlow)
  
  coler <- "transparent"
  coler[idhigh==ncol(x)] <- palette[1]
  coler[idlow==ncol(x)] <- palette[2]
  
  if (lang=="en") groups <- c("High","Low")
  if (lang=="es") groups <- c("Alto","Bajo")
  if (lang=="pt") groups <- c("Alto","Baixo")
    
  plot(spobj, col=coler, border=border)
  if (! is.null(frame)) plot(frame, lwd=lwd.frame, add=T)  

  adleg(legpos = legpos,cex.leg = cex.leg, palette = palette, groups = groups, classint = "unique",...)

}




##### image-dot map - dot density map using images ####

# The imgmap() function creates a dot density map using PNG files to represent points.

# Parameters:
#             spobj - spatial object containing the points to be drawn.
#             file - image filename 
#             frame - spatial object used to frame the image dot density map
#             add - logical - adds the current map to a pre-existing one.

imgmap <- function(spobj, file=NULL, frame=NULL, add=T){
  require(png)
  require(TeachingDemos)
  coord <- coordinates(spobj)
  img <- readPNG(file)
  par(mar=rep(0,4))
  plot(frame)
  my.symbols(coord[,1], coord[,2], ms.image, MoreArgs=list(img=img), inches=0.1, symb.plots=TRUE, add=add)
}



########################################################################################
########### Symbol map ################################3

# NECESSITY OF CONSTRUCTING A GOOD LEGEND FUNCTION TO DEAL WITH PROPORTIONAL SYMBOLS AND DIFFERENT SYMBOLS

## This function allows to use symbols from True Type Fonts in maps. Much quicker than using image files.

# Parameters:
#             spobj -
#             fontname -
#             pch -
#             frame -
#             cex -
#             col - 

simbmap <- function(spobj=NULL, fontname="Wingdings", pch="Q",frame=NULL, cex=1, col="black"){

  windowsFonts(FAM1 = windowsFont(fontname))
  
  if (!class(spobj) %in% c("SpatialPoints","SpatialPointsDataFrame")){
    spobj <- SpatialPointsDataFrame(spobj, data.frame(spobj))
  }
  
  par(family="FAM1")
  par(mar=rep(0,4))
  plot(frame)
  plot(spobj, pch=pch, cex=cex, col=col, add=T)
  plot(frame, add=T)
  par(family="")
  
}








##### Proportional symbol map - dot density map using images ####

# This function generates a proportional symbol map combining it with a classification method tha colours the symbols according to the parameter passed to the colmap() function.

# Parameters:
#             spobj - spatial object used to generate the map
#             var - variable containing the values to be represented
#             type - the type of calculation to determine the size of the circles. The possible values are: "volume", "surface", and "perceptual". The default is perceptual.
#             frame - spatial object used as frame to the points
#             percep - logical - if TRUE uses perceptual algorithm to define the size of symbols
#             max.size - maximum size of symbols
#             col - color of simbols used
#             pch - point character employed
#             leg.brks - number of breaks in the legend
#             horiz - logical - defines if the legend will be horizontal or vertical
#             leg.cex - size of characters on the legend
#             dec - decimals used to represent values in the legend
#             add - logical - adds this proportional symbol map to a previous one
#             leg.pos - legend position.
#             y.intersp - 
#             x.intersp -
#             varclass - variable used to the classification of cases
#             palet - RColorBrewer color palet employed
#             classint - classification method used (see colmap)
#             nbrks - number of breaks
#             invpalet - invert the order of color palet (see colmap and mapa)
#             lex.aux - position of auxiliary legend
#             title.leg - title of main legend
#             title.aux - title of auxiliary legend
#             title.adj - adjustment of the text of legend titles
#             custcol - custom colors (when do not wish to use RColorBrewer palettes)


propmap <- function(spobj, var=NULL, frame=NULL, type="perceptual", max.size=5, pch=21, leg.brks=3, horiz=F, horiz.aux=F, leg.cex=1.8, leg.aux.cex=1.8,dec=0, add=F, leg.pos="bottomleft", y.int=0.5, x.int=NULL, varclass=NULL, palet="Blues", classint="jenks", nbrks=5, invpalet=F, leg.aux="bottomright",title.leg=NULL, title.aux=NULL, title.adj=0.5, custcol=NULL, big.mark=".", decimal.mark=",", y.int.aux=0.5, x.int.aux=0.2, yjust=0.5,transp=99, userbrk=NULL,symbol.fg="black", symbol.bg="red",font="Times New Roman",dec.aux=1,...){

  windowsFonts(times=windowsFont(font))
  par(family="times")  
  
  leg.brks <- leg.brks + 1
  
  require(classInt)
  
  if (class(spobj)=="SpatialPolygonsDataFrame") spobj <- SpatialPointsDataFrame(spobj, data.frame(spobj))
  
  var[is.na(var)] <- 0
  
  switch(type, volume = spobj$pval <- ((var/max(var,na.rm=T))^(1/3)) * max.size, surface = spobj$pval <- sqrt(var/max(var,na.rm=T)) * max.size, perceptual = spobj$pval <- ((var/max(var,na.rm=T)) * max.size))
leg.brks <- 3

  if (is.null(userbrk)){
    rd <- classIntervals(var=var, n=leg.brks, style=classint)
    rd <- rd$brks[2:length(rd$brks)]
  }else{
    rd <- userbrk[1:length(userbrk)]
  }
  
  switch(type, volume = ri <- ((rd/max(var,na.rm=T))^(1/3)) * max.size, surface = ri <- sqrt(rd/max(var,na.rm=T)) * max.size, perceptual = ri <- ((rd/max(var,na.rm=T)) * max.size)  )  

  

  if (! is.null(varclass)){
    varclass[is.na(varclass)] <- 0
    cores <- colmap(varclass,palet=palet, classint=classint,nbrks=nbrks, invpalet=invpalet, custcol=custcol, transp=transp)
    spobj$col <- cores[[1]]
    legs.aux <- as.numeric(cores[[2]])
    pal <- cores[[3]]
    col <- pal[length(pal)]
  }else{
    col <- rgb(t(col2rgb(symbol.bg)), maxColorValue=255)
    spobj$col <- paste(col, transp, sep="")
  } 
  
  
#   spobj@plotOrder<- order(sort(spobj$pval, decreasing=T))
  spobj <- spobj[order(spobj$pval,decreasing=T),]
  
  if(! is.null(frame)){
    plot(frame)
    plot(spobj, pch=pch, cex=spobj$pval, bg=spobj$col, fg=symbol.fg, add=T)
  }else{
    plot(spobj, pch=pch, cex=spobj$pval, bg=spobj$col, fg=symbol.fg, add=add)
  }
  
  legs <- sort(rd, decreasing = T)
  ri <-sort(ri, decreasing = T)

  legs <- round(legs,dec)
  legs <- format(round(legs, dec), big.mark=big.mark, decimal.mark=decimal.mark)
  
  if(is.null(x.int)) x.int <- ri/max.size*0.7

  adleg(legpos = leg.pos, leg.main = title.leg, cex.leg = leg.cex, pt.cex=ri, y.intersp = y.int,x.intersp= x.int, pch = pch, classint = "unique", val = var,palette = palette,userbrk = userbrk,groups = legs, dec = dec, cl = "SpatialPoints", big.mark = big.mark, decimal.mark = decimal.mark, horiz = horiz, add = T)


  if (! is.null(varclass)){
  adleg(leg.aux, leg.main = title.aux, cex.leg = leg.aux.cex, y.intersp = y.int.aux, x.intersp = x.int.aux, classint = classint, val = var, palette = pal, cl = "SpatialPolygons", userbrk = userbrk, groups =legs.aux, big.mark = big.mark, decimal.mark = decimal.mark, horiz = horiz, add = T, dec = dec.aux)
  }

#   par(family="")
#   
}




##### Kernel Map #####
# Performs a kernel density map of a point object

# Parameters:
#             sppoint - spatial point object used in the calculation
#             spwindow - spatial polygon object used as window to calculate the kernel
#             frame -
#             plot -
#             return -
#             res -
#             alpha - 
#             diggle -
#             edge - 

kernelmap <- function(sppoint=NULL, spwindow=NULL, frame=NULL, plot=T, return=F, res=480, alpha=0.5, diggle=T, edge=T, title=NULL, nwhite=20, border.col="gray70", transp=NULL, backcol="#FFFFFF", legpos="bottomleft", leg.cex=1.2, add=F){
  
  # load the package spatsat
  require(spatstat)
  

  # Set the checking option off in order to create the window object
  # spatstat.options(checkpolygons=FALSE)
  
  # Generates the window which will frame the results
  spowin <- as.owin.SpatialPolygons(spwindow)
  
  # Set the checking option ON after creating the window object
  # spatstat.options(checkpolygons=TRUE)
  
  # Retrieve the coordinates of airports
  coords <- coordinates(sppoint)
  
  #Generate the point object used to create the density map
  ptobj <- as.ppp(coords, W=spowin)
  
  # Calc. the density map - it can take some time because the grid resolution is medium-high (dimyx=480)
  dens <- density.ppp(ptobj, alpha, diggle=diggle, edge=edge, dimyx=res)
  
  
  if (plot==T & ! is.null(frame)){
    
    
    # Define the color palette
    colores <- heat.colors(100)
    
    #Plot the base map
#     par(mfrow=c(1,1))
    
    if (! is.null(title)){
      par(mar=c(0,0,2,0)) 
      plot(frame, border=border.col, col=backcol, bg=backcol, add=add)
      title(title)
    }else{
      par(mar=rep(0,4))
      plot(frame, border=border.col, col=backcol, bg=backcol, add=add)}
    
    colo <- colores[100:1]
    colo[1:nwhite] <- backcol

    if(! is.null(transp)) colo <- paste0(colo,transp)

    # Plot the results
    plot(dens, col=colo, add=TRUE)
    
    # Superpose the frame object as a contextual frame
    plot(frame, border=border.col, add=TRUE)
  }
  
#   image.plot(legend.only=T, zlim=c(0,1), col= heat.colors(100)[100:1], smallplot=c(0.05,0.08,0.05,0.25),axis.args=list(at=c(0,1), labels=c("Baja","Alta")))
  

legend(legpos, legend = c("Alta",rep("",6),"Baja"), fill = heat.colors(8), y.intersp = 0.5, border = "transparent", cex=leg.cex, bty = "n")


  if(return==T)  return(dens)
  
  
}

##### Non-contiguous area-based cartogram (Hisaji ONO)
# Extracted from: http://www.mail-archive.com/r-sig-geo@stat.math.ethz.ch/msg00185.html

# Creates a non-contiguous area-based cartogram.
# Parameters:
#             spobj - spatial object to be used as base to create the cartogram
#             val - a continuous variable with the same length as spobj
#             ret.obj - indicates wether a spatial object will be returned or not
#             exp - expansion factor (to correct for extremely low values)

cartograma <- function(spobj=NULL, val=NULL, ret.obj=F, exp=1){
  
  require(rgdal)
  require(rgeos)
  
  coords <- SpatialPolygons2PolySet(spobj)
  
  densities<- sqrt(val/gArea(spobj,byid=T))
  
  maxDensity<-max(densities, na.rm=T)
  k <- 1/maxDensity
  L <- k*densities
  
  L[is.na(L)] <- min(L, na.rm=T)
  L <- L*exp
  
  centroidXY<-coordinates(spobj)
  
  dt <- data.frame(cbind(L, centroidXY[,1],centroidXY[,2]))
  names(dt) <- c("L","CTX","CTY")
  
  dt$PID <- seq(1,nrow(centroidXY))
  
  dt_coord <- merge(coords,dt, by="PID")
  
  dt_coord$newX <- dt_coord$L*(dt_coord$X-dt_coord$CTX)+dt_coord$CTX  
  
  dt_coord$newY <- dt_coord$L*(dt_coord$Y-dt_coord$CTY)+dt_coord$CTY
  
  res <- dt_coord[,c(1,2,3,9,10)]
  names(res) <- c("PID","SID","POS","X","Y")
  
  
  sppol <- as.PolySet(res,projection="UTM", zone=23)
  
  cartopol <- PolySet2SpatialPolygons(sppol, close_polys=TRUE)
  
  plot(spobj)
  plot(cartopol, col="red", add=T, border="transparent")
  
  if (ret.obj==T) return(cartopol)
  
}



# Mapa de competencia electoral


compmap <- function(spobj, vara_t0, vara_t1, varb_t0,varb_t1, lang="es", leg=c("Party A","Party B"), border="transparent", frame=NULL, leg.pos="bottomleft",leg.cex=1.2, y.intersp=0.5, palette="Spectral", inv.pal=F){
  
  require(RColorBrewer)
  
 pal <- brewer.pal(6, palette)

 if (inv.pal==TRUE) pal <- pal[length(pal):1]
#   pal <- c("red2","Orange","gold","lightskyblue","cornflowerblue","blue")
#   pal <- c("#B2182B","#EF8A62","#FDDBC7","#D1E5F0","#67A9CF","#2166AC")  
#   pal <- c("#D53E4F","#FC8D59","#FEE08B","#E6F598","#99D594","#3288BD")  

  adif <- vara_t1-vara_t0
  bdif <- varb_t1-varb_t0
  
  ganat0 <- 0
  ganat0[vara_t0>varb_t0] <- 1
  ganat0[is.na(ganat0)] <- 0
  
  ganat1 <- 0
  ganat1[vara_t1>varb_t1] <- 1
  ganat1[is.na(ganat1)] <- 0
  
  dif <- NA
  dif[adif>0 & ganat0==1 & ganat1==1] <- 1 
  dif[adif<0 & ganat0==1 & ganat1==1] <- 2 
  dif[ganat0==0 & ganat1==1] <- 3 
  dif[ganat0==1 & ganat1==0] <- 4 
  dif[bdif<0 & ganat0==0 & ganat1==0] <- 5 
  dif[bdif>0 & ganat0==0 & ganat1==0] <- 6 
  
  ta <- table(c(1,2,3,4,5,6))
  tb <-table(dif)
  ta[! names(ta) %in% names(tb)] <- 0
  ta[ names(ta) %in% names(tb)] <- tb
  
  tbp <- round(ta/sum(ta)*100,1)

  if (lang=="en") lgval <- c("Increase margin ","Decrease margin ", " to ")
  if (lang=="es") lgval <- c("Vict. con más margen ","Vic. con menos margen ", " a ")
  if (lang=="pt") lgval <- c("Aumenta margem ","Diminui margem ", " a ")
  
  legs <- c(paste0(lgval[1], leg[1]),paste0(lgval[2], leg[1]),paste0(leg[2],lgval[3],leg[1]),paste0(leg[1],lgval[3],leg[2]),paste(lgval[2], leg[2]),paste(lgval[1], leg[2]))
  
  legs <- paste(legs," (",ta,"/",tbp,"%)", sep="")
  
  cols <- pal[findInterval(dif,sort(unique(dif)))]
  
  par(mar=rep(0,4))
  plot(spobj, border=border, col=cols)
  if(! is.null(frame)) plot(frame, add=T)
  
  legend(leg.pos, legend=legs, fill=pal, bty="n", y.intersp=y.intersp, cex=leg.cex)
  
}








