# R code to visualize spatial pattern of species area changes

# Unraveling Global Impacts of Climate Change on Amphibians Distributions: 
# A Life-History and Biogeographic-Based Approach (paper in review)

library(maps)
library(dplyr)
library(raster)
library(terra)

## shapefile of IUCN
file.choose()
range_shp <- sf::st_read("/home/gabriela/Documentos/Doutorado_UESC/Artigo_Revisao_Pasta_Drive/Artigo_revisao-20220816T123038Z-001/Artigo_revisao/AmphiBIO_v1/data/data_0.shp") 
head(range_shp)

## species names
file.choose()
sp <- read.csv( "/home/gabriela/Documentos/Doutorado_UESC/Artigo_Revisao_Pasta_Drive/Artigo_revisao-20220816T123038Z-001/Artigo_revisao/AmphiBIO_v1/output/data_complete_data_localities.csv", header = TRUE) 
head(sp)

## is there a difference in species names between your database 
## and iucn database?
setdiff(sp$specie_match_iucn, range_shp$BINOMIAL)

## mean relative prop per species
sp_mean <- data.frame(prop_rel = tapply(sp$prop_rel, 
                                        sp$specie_match_iucn, mean))
speciesnames <- rownames(sp_mean)
sp_mean <- data.frame(sp_mean, species = speciesnames)
rownames(sp_mean) <- NULL
head(sp_mean)
summary(sp_mean)

## cut the IUCN shape for species of interest 
{
  range_ok <- merge(x = range_shp, y = sp_mean, by.x = "BINOMIAL", 
                    by.y = "species")
  head(range_ok)
  shapes <- range_ok %>% as("Spatial")
  shapest <- vect(shapes) # spatvector
  crs <- "+proj=longlat +ellps=WGS84"
  shapest <- project(shapest, crs)
  plot(shapest)
}

# loop to generate one raster per species
{
  # rasterize shapefile
  exte <- ext(shapest) # extent
  rr <- rast(exte, resolution = 0.5) # extent raster
  r_list <- list() # list to store the objects created in a loop for
  for(i in 1:length(unique(shapest$BINOMIAL))){
    r_list[[i]] <- terra::rasterize(shapest[i,], rr) 
  }
  rt <- rast(r_list) # raster stack
  names(r_list) <- unique(shapest$BINOMIAL) # names
  plot(rt, col = "green")
}

{
  # sorting the vector of proportions to match the species order in the raster
  sp_mean$species <- sort(sp_mean$species)
  sp_mean <- sp_mean[-79,] # removing Elachistocleis cesarii(not in the shape)
  names(rt) <- sort(names(rt))
  # testing if names match
  sp_mean$species == sort(names(rt))
  cbind(sp_mean$species, names(rt))
}

## Spatial pattern of species area changes  
{
  # w = proportion vector
  # x = SpatRaster
  
  ### median
  {
    f <- function(x, w, na.rm = TRUE) median((x)*w, na.rm = na.rm)
    s <- app(rt, f, w = sp_mean$prop_rel, na.rm = TRUE)
    
    # collapsing values greater than 2.3 to 2.3
    fi <- function(x) ifelse(x > 2.31234, 2.31234, x)
    sc <- app(s, fi)
    
    # median map
    breaks <- seq(0, 2.1, by = 0.1)
    colors <- colorRampPalette(c("red", "gray", "blue"))(length(breaks)-1)
    breaks2 <- seq(2.1, 2.3, by = 0.1)
    colors2 <- c(colorRampPalette(c("blue2","navy"))(length(breaks2) - 1),
                 "purple")
    
    x11()
    hist(s) # more frequent values
    plot(sc, col= c(colors, colors2), range  = c(0, 2.4),
         plg = list(at = c(0, 1, 2, 2.3), labels=c(0, 1, 2, ">=2.3")))
    maps::map(add = TRUE)
  }
  
  ### quantiles map used in the paper
  {
    # function to calculate quantiles to vector
    fq <- function(x, w, na.rm = TRUE, probs) quantile((x)*w, na.rm = na.rm, probs = probs)
    # applying in raster
    sq <- app(rt, fq, w = sp_mean$prop_rel, na.rm = TRUE, 
              probs = seq(0, 1, 0.25))
    # if you can save the results
    writeRaster(sq, "quantiles_maps.tiff")
    
    ## breaks to define colors in the map
    breaksq <- seq(0, 2, by = 0.1)
    colorsq <- colorRampPalette(c("red", "gray", "blue"))(length(breaksq)-1)
    breaks2sq <- seq(2, 21.28197, by = 0.1)
    colors2sq <- c(colorRampPalette(c("blue3","navy", "purple"))
                   (length(breaks2sq) - 1), "purple")
    
    ## plot
    x11()
    e <- c(-150, 150, 75, 80) # local to plot legend
    plot(sq[[1,]], col = c(colorsq, colors2sq), range  = c(0, 21.28197),
         plg = list(at = c(0, 1, 2, 5, 10, 15, 20), ext = e, loc = "bottom",
                    labels = c(0, 1, 2, 5,10, 15, 20)))
    maps::map(add = TRUE, interior = TRUE)
    
    ## loop for plot each quantile
    x11(record = TRUE)
    par(mfrow = c(3,2))
    dev.off()
    for(i in 1:(nlyr(sq))){
      plot(sq[[i,]], col = c(colorsq, colors2sq), range  = c(0, 21.28197),
           legend = FALSE,
           axes = TRUE, main = c("0%", "25%", "50%", "75%", "100%")[i]
           #plg = list(at = c(0, 1, 2, 5, 10, 15, 20), ext = e, loc = "top",
           #labels = c(0, 1, 2, 5,10, 15, 20))
      )
      maps::map(add = TRUE, interior = TRUE)[i]
    }
  }
}

## richnnes map
{
  ## function to calculate the richness map
  rq <- app(rt, sum, na.rm = TRUE)
  hist(rq) # most frequent values
  
  ## breaks to define colors in the map
  breaksr <- seq(1, 30, by = 0.1)
  colorsr <- colorRampPalette(c("gray", "blue3", "red"))(length(breaksr)-1)
  breaks2r <- seq(2.1, 2.3, by = 0.1)
  colors2r <- c(colorRampPalette(c("blue","navy"))(length(breaks2r) - 1),
                "purple")
  
  ## plot
  x11()
  e <- c(-150, 150, 93, 97) # where plot legend
  plot(rq, col=c(colorsr), range  = c(1, 30), axes = F,
       plg = list(at = c(1, 5, 10, 15, 20, 25, 30), 
                  ext = e, loc = "bottom",
                  labels = c(1, 5, 10, 15, 20, 25, 30)))
  maps::map(add = TRUE) # world countries map
}