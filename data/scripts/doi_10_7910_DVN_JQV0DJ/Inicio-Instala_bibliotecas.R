# Install all R packages required to perform the exercises and examples
install.packages(c("RColorBrewer","sp","spatstat","maptools", "classInt","fields","PBSmapping","maps","pycno","rgeos","sf","spdep","splancs","tmap","tmaptools"))


# Loads the basic package for opening spatial data
library(rgdal)

# Loads the spatial data (description in the data.frame called desc)
load("Data/Dados_Espaciais.RData")


# Opens a random spatial object to examine

# Polygon
class(e)

# plot the spatial object
par(mar=rep(0,4))
plot(e)



# Point
class(a)

# plot the spatial object
par(mar=rep(0,4))
plot(a)


# Lines
class(f)

# plot the spatial object
par(mar=rep(0,4))
plot(f)


# All at once

par(mar=rep(0,4))

plot(e)

plot(a, pch="o", cex=1, add=T, col="red")

plot(f, col="darkgreen", add=T)

plot(f[f$SITUACAO=="OPERAÃ‡ÃƒO",], col="blue", lwd=2, add=T)

plot(e, lwd=2, add=T)



# Making maps

# 1 - asociating colors to values

# Pre-defined color schemes
library(RColorBrewer)

# All the color schemes
display.brewer.all()

# Chose the "Blues" with five colors
pal <- brewer.pal(5, "Blues")


# Classify cases according to their value 
library(classInt)

# Examine the size of government variable
e$GOV_SIZE

# Divide the variable into 5 groups (quantiles)
groups <- quantile(e$GOV_SIZE, probs = seq(0,1,0.2))

groups

# Assign a group to each value
order <- findInterval(x = e$GOV_SIZE, vec = groups, all.inside = T)

order

# Assign colours to each value
col <- pal[order]

col

# Plot the results
par(mar=rep(0,4))
plot(e, col=col)


# Merging data to spatial objects

# Brazilian municipalies basic shapefile

# Inspect the data linked to each spatial object
View(data.frame(b))


# Open data on presidential elections
load("Data/BR_Pres_vote.RData")

# Inspect the data
View(br)

# Filter some data for making the merge
pt <- br[br$year==2014 & br$party==13,]


# Merge the data with the spatial object
pt14 <- merge(b, pt, by="GEOCODIG_M")


# Inspect the results
View(data.frame(pt14))


# Repeat the operation to assign colors to values in order to make a map

# Chose the "Reds" with five colors
pal <- brewer.pal(5, "Reds")


# Divide the variable into 5 groups (quantiles)
groups <- quantile(pt14$prop_votes, na.rm = T, probs = seq(0,1,0.2))

groups

# Assign a group to each value
order <- findInterval(x = pt14$prop_votes, vec = groups, all.inside = T)

order

# Assign colours to each value
col <- pal[order]

col

# Plot the results
par(mar=rep(0,4))
plot(pt14, col=col)

# Not good, let's try something different
par(mar=rep(0,4))
plot(pt14, col=col, border="transparent")
plot(e, add=T, lwd=2)


# Repeating is boring, let's make a function

map <- function(spobj, val, palette="Blues", nbrks=5, frame=NULL){
  
  # Chose the "Reds" with five colors
  pal <- brewer.pal(nbrks, palette)
  
  
  # Divide the variable into 5 groups (quantiles)
  groups <- quantile(val, na.rm = T, probs = seq(0,1,(1/nbrks)))
  

  # Assign a group to each value
  order <- findInterval(x = val, vec = groups, all.inside = T)
  

  # Assign colours to each value
  col <- pal[order]
  

  # Plot the results
  par(mar=rep(0,4))
  plot(spobj, col=col)
  
  # Not good, let's try something different
  par(mar=rep(0,4))
  plot(spobj, col=col, border="transparent")
  
  if (! is.null(frame))  plot(frame, add=T, lwd=2)
  

}



map(pt14, pt14$prop_votes)


map(pt14, pt14$prop_votes, palette="Greens", nbrks=4, frame=e)


