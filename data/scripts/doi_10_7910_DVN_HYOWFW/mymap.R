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
  
  if(is.null(lwobj) & class(spobj) %in% c("SpatialPolygonsDataFrame", "SpatialPolygons")){
    lwobj <- poly2nb(spobj[! is.na(val),], queen=T)
    lwobj <- nb2listw(lwobj,zero.policy=T)
  }
  
  
  lcm <- localmoran(val, lwobj)
  

  # Z-value confidence interval
  zlim <- qnorm(1-alpha)
  zlima <- -1*zlim

  if (! is.null(val)){  
    val[is.na(val)] <- mean(val, na.rm=T)
    zx <- scale(val, center=T,scale=T)

    if (type %in% c("i","g")){
      if (is.null(val2)) lagzx <- scale(lag.listw(lwobj, val[! is.na(val)]), center=T,scale=T)
      if (! is.null(val2)) {
        val2[! is.na(val2)] <- mean(val2, na.rm=T)
        lagzx <- scale(lag.listw(lwobj, val2[! is.na(val2)]), center=T,scale=T)}
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


