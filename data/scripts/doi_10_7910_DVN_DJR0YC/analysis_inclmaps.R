

#####################################
## SIDE-EPR-INCLUSION MAP ###########
#####################################

# INIT
library(viridis)
library(plyr)
library(cshapes)
library(raster)
library(countrycode)
library(ggplot2)

# LOAD DISTRICTS 2000 (called GAUL.spdf)
load("data/districts_2000.RData")
dist.cov.full <- readRDS("data/districts_2000_covariates.rds")


# Cshapes for borders
cshp <- cshp(date = as.Date("2000-1-1"))

# DRAW SOME MAPS OF DISTRICT-YEARS

# Select latest available SIDE data
dist.data <- join(dist.cov.full, 
                  aggregate.data.frame(list(max.year = dist.cov.full$side.year,
                                            max.round = dist.cov.full$dhs_round,
                                            max.subround = dist.cov.full$dhs_subround),
                                       dist.cov.full[,c("cowcode", "year")], FUN = max),
                  type = "left",
                  by = c("cowcode", "year"))
dist.data <- dist.data[dist.data$side.year == dist.data$max.year & 
                         dist.data$dhs_round == dist.data$max.round & 
                         dist.data$dhs_subround == dist.data$max.subround,]

# Set colours right
palette <- rev(viridis(256, option = "inferno"))
dist.data$colkey <- palette[as.numeric(cut(dist.data$epr.incl.dist, 
                                           seq(0,1,length.out = 256), 
                                           include.lowest = T))]

# Plot all countries in 2000 (Figure 1 and Appendix maps)
for(c in na.omit(unique(dist.data$cowcode))){
  # print(c)
  # Data
  df <- join(gaul.spdf@data[gaul.spdf$cowcode == c,], 
             dist.data[dist.data$cowcode == c & dist.data$year == ifelse(c == 451, 2002, 2000) &
                         !is.na(dist.data$epr.incl.dist),], type = "left",
             by = c("cowcode", "id.new"))
  plot.spdf <- SpatialPolygonsDataFrame(gaul.spdf[gaul.spdf$cowcode == c,],
                                        data = df, match.ID = F)
  # Plot
  this.ext <- as.vector(extent(plot.spdf))
  png(file.path(fig.path, paste0("incl_map2000_",c,".png")), 
      width = 7* ((this.ext[2] - this.ext[1])/(this.ext[4] - this.ext[3])), height = 7, 
      res = 300, unit = "in")
  par(mar = c(0,0,0,0))
  plot(plot.spdf,col = plot.spdf$colkey,
       border="white", lwd = 0.5)
  dev.off()
}

# Plot Uganda over time (Figure 2)
c = 500
for(y in c(1965, 1980, 1995)){
  # Data
  df <- join(gaul.spdf@data[gaul.spdf$cowcode == c,], 
             dist.data[dist.data$cowcode == c & dist.data$year == y &
                         !is.na(dist.data$epr.incl.dist),], type = "left",
             by = c("cowcode", "id.new"))
  plot.spdf <- SpatialPolygonsDataFrame(gaul.spdf[gaul.spdf$cowcode == c,],
                                        data = df, match.ID = F)
  # Plot
  this.ext <- as.vector(extent(plot.spdf))
  png(file.path(fig.path, paste0("incl_map",y,"_",c,".png")), 
      width = 7* ((this.ext[2] - this.ext[1])/(this.ext[4] - this.ext[3])), height = 7, 
      res = 300, unit = "in")
  par(mar = c(0,0,0,0))
  plot(plot.spdf,col = plot.spdf$colkey,
       border="white", lwd = 0.5)
  dev.off()
}


# Palette image
png(file.path(fig.path, "incl_map2000_palette.png"), width = 1, height = 7, unit = "in",res = 300)
par(mar = c(0.8,0,0.8,2))
graphics::image(1, seq(0,1, length.out = 256), t(c(1:256)), col=palette, xlim = c(0.9,1.1), axes=FALSE,
                xlab = "", ylab = "")
axis(4)
dev.off()



# All Africa  
df <- join(gaul.spdf@data, 
           dist.data[!is.na(dist.data$epr.incl.dist) & dist.data$year == 2000,], type = "left",
           by = c("cowcode", "id.new"))
plot.spdf <- SpatialPolygonsDataFrame(gaul.spdf,
                                      data = df, match.ID = F)

# ... plot
png(file.path(fig.path, "incl_map2000.png"), 
    width = 7, height = 6, res = 300, unit = "in")
par(mar = c(0,0,0,0))
layout(matrix(1:2,ncol=2), width = c(6,1),height = c(1,1))
# xmin        : -25.36055 
# xmax        : 63.49575 
# ymin        : -46.96973 
# ymax        : 37.34041 
plot(cshp[cshp$COWCODE %in% c(400:626,651),], border = "black", 
     lwd = 0.75, xlim = c(-14,50), ylim = c(-35,36))
plot(plot.spdf, 
     col = plot.spdf$colkey,
     border="white", lwd = 0.25,
     add = T)
legend_image <- as.raster(matrix(rev(palette), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
axis(side = 4,pos = 0.5)
rasterImage(legend_image, 0,0, 
            0.5, 1)
dev.off()


#######################
# DHS Map Africa Figure A4


# Plot
png(file.path(fig.path, "dhs_points.png"), 
    width = 6, height = 6, res = 300, unit = "in")
par(mar = c(0,0,0,0))
plot(cshp[cshp$COWCODE %in% c(400:626,651),], 
     lwd = 0.75, xlim = c(-14,50), ylim = c(-35,36))
points(unique(data[data$cowcode != 439,c("longnum","latnum")]), pch = ".", col = "red")
dev.off()


#######################
# EPR INCLUSION / EXCLUSION TIMELINE Figure A5

# New: GET EPR From data repository 

## Download
epr.group <- read.csv("https://icr.ethz.ch/data/epr/core/EPR-2014.csv", stringsAsFactors = F)

## Drop non-african vars
epr.group <- epr.group[epr.group$gwid %in% na.omit(unique(dist.data$cowcode)), ]

## Recode status
status.vec <- c("MONOPOLY","DOMINANT", "SENIOR PARTNER", "JUNIOR PARTNER", "POWERLESS",  "DISCRIMINATED" , 
                "SELF-EXCLUSION", "IRRELEVANT", "STATE COLLAPSE")
epr.group$statusid <- NA
for(s in status.vec){
  epr.group$statusid[epr.group$status == s] <- which(status.vec == s)
}
epr.group$included <- ifelse(epr.group$statusid <= 4, 1, 0)

epr.group <- epr.group[!is.na(epr.group$included),]
epr.group$status.fac <- factor(epr.group$status, levels = status.vec, ordered = T)

# Make Plot data
plot.df <- epr.group
plot.df <- plot.df[order(plot.df$statename, plot.df$group, decreasing = T),]

## State & group positions
count <- 1
sep.lines.y <- c()
plot.df$ypos <- unlist(lapply(1:nrow(plot.df), function(i){
  if(i == 1){
    count
  } else {
    if(plot.df$statename[i] != plot.df$statename[i-1]){
      count <<- count + 1
      sep.lines.y <<- c(sep.lines.y, count)
    }
    if(plot.df$group[i] != plot.df$group[i-1]){
      count <<- count + 1
    }
    count
  }
}))
# plot.df$ypos <- abs(max(plot.df$ypos) - plot.df$ypos)
plot.df$group.clean <- plot.df$group # gsub("\\(.*?\\)", "", plot.df$group)
plot.df$group.clean[grepl("Northern (Bariba", plot.df$group.clean, fixed = T)] <- "Northern (Bariba, Peul, Ottamari, etc.)"
plot.df$group.clean[grepl("Northerners (Langi, Acholi, Teso, Madi,", plot.df$group.clean, fixed = T)] <- "Northerners (Langi, Acholi, Teso, Madi, etc.)"
plot.df$group.clean[grepl("Northerners (Langi, Acholi, Madi, ", plot.df$group.clean, fixed = T)] <- "Northerners (Langi, Acholi, Madi, etc."
plot.df$group.clean[grepl("KabrÃ© (and related groups)", plot.df$group.clean, fixed = T)] <- "Kabré (and related groups)"


## Statenames
state.plot <- aggregate(list(ypos = plot.df$ypos),
                        list(statename = plot.df$statename), FUN = mean)
state.plot$statename <- gsub(" ", "\n", state.plot$statename, fixed = T)
state.plot$statename[state.plot$statename  == "Mozambique"] <- "Mozam-\nbique"

# Plot
png(file.path(fig.path, "epr_timeline_1.png"), 
    width = 7, height = 10, res = 400, unit = "in")
this.min.y <- min(plot.df$ypos[plot.df$statename == "Kenya"])
ggplot(plot.df[plot.df$ypos >= this.min.y & plot.df$included == 1,], aes(y = ypos)) +
  geom_segment(aes(y = ypos, yend = ypos, x = from -.5, xend = to + .5), lwd = 3, color = "darkgrey") +
  geom_text(data = state.plot[state.plot$ypos >= this.min.y,], aes(x = 1940, label = statename), angle = 90,
            lineheight = 1) +
  geom_text(data = unique(plot.df[plot.df$ypos >= this.min.y,c("group.clean","ypos")]), aes(x = 2014, label = group.clean), hjust = 0, size = 3) +
  geom_segment(data = data.frame(ypos = sep.lines.y[sep.lines.y > this.min.y]), 
               aes(y = ypos, yend = ypos, x = 1945, xend = 2013), color = "darkgrey", lty = 2) +
  theme_minimal() +
  theme(legend.position = "top") + labs(x="", y="") +
  scale_x_continuous(breaks = seq(1950, 2010, by = 10), limits = c(1940, 2080)) + 
  scale_y_continuous(expand=c(.02,.02), breaks = NULL) +
  NULL
dev.off()


png(file.path(fig.path, "epr_timeline_2.png"), 
    width = 7, height = 10, res = 400, unit = "in")
this.min.y <- min(plot.df$ypos[plot.df$statename == "Kenya"])
ggplot(plot.df[plot.df$ypos < this.min.y & plot.df$included == 1,], aes(y = ypos)) +
  geom_segment(aes(y = ypos, yend = ypos, x = from -.5, xend = to + .5), lwd = 3, color = "darkgrey") +
  geom_text(data = state.plot[state.plot$ypos < this.min.y,], aes(x = 1940, label = statename), angle = 90, size = 3.5,
            lineheight = 1) +
  geom_text(data = unique(plot.df[plot.df$ypos < this.min.y,c("group.clean","ypos")]), aes(x = 2014, label = group.clean), hjust = 0, size = 3) +
  geom_segment(data = data.frame(ypos = sep.lines.y[sep.lines.y < this.min.y -3]), 
               aes(y = ypos, yend = ypos, x = 1945, xend = 2013), color = "darkgrey", lty = 2) +
  theme_minimal() +
  theme(legend.position = "top") + labs(x="", y="") +
  scale_x_continuous(breaks = seq(1950, 2010, by = 10), limits = c(1940, 2080)) + 
  scale_y_continuous(expand=c(.02,.02), breaks = NULL) +
  NULL
dev.off()



