
options(stringsAsFactors = FALSE)

#general
library(tidyverse)
library(cobalt)  #f.build function

#file types
library(readxl)     #files: excel
library(foreign)

#table
library(xtable)
library(stargazer)
library(tools)  #toTitleCase
library(rlist)  #append to an existing list

hijack <- function (FUN, ...) {
    .FUN <- FUN; args <- list(...)
    invisible(lapply(seq_along(args), function(i) {
        formals(.FUN)[[names(args)[i]]] <<- args[[i]]
    })); .FUN }

stargazer <- hijack(stargazer, table.placement="H", 
                    omit.stat=c("f", "adj.rsq", "ser"), 
                    dep.var.caption="",
                    dep.var.labels.include = TRUE,
                    type="latex",
                    digits=2,
                    font.size = "footnotesize",
                    header = FALSE)


#robust standard errors
library(lmtest)
library(sandwich)    

p.value <- function(x) ifelse(x <.01, "$^{***}$", 
                              ifelse(x < .05, "$^{**}$", 
                                     ifelse(x<.1, "$^{*}$","")))


#mapping
library(raster)     #mapping: rasters
library(maptools)   #mapping: plotting
library(rgeos)      #mapping: simplifying geometry
library(rgdal)      #mapping: projecting
library(spdep)      #mapping: spatial statistics
library(rnaturalearth)   #API to natural earth data
library(ape)  #moran's i
library(countrycode)
unproj <- CRS("+proj=longlat +datum=WGS84") #default WGS84 projection
proj <- CRS("+init=epsg:20135")  #South Sudan

#plotting
library(ggplot2)        #graphics: ggplot
library(RColorBrewer)   #plotting: color scales
library(scales)
library(grid)
library(gridExtra)

red <- hue_pal()(3)[1]
green <- hue_pal()(3)[2]
blue <- hue_pal()(3)[3]
purple <- hue_pal()(4)[4]
