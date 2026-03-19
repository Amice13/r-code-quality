### phyloraster: an R package to calculate spatialized measures of endemism and evolutionary diversity
# Case study used in the manuscript

# install and load devtools to install phyloraster
install.packages("devtools")
devtools::install_github("gabferreira/phyloraster", force = TRUE)

# load the package
library(phyloraster)

## load and visualize the dataset from phyloraster
ras <- terra::rast(system.file("extdata", "rast.presab.tif", package="phyloraster"))
plot(ras)

tree <- ape::read.tree(system.file("extdata", "tree.nex", package="phyloraster"))

## prepare the dataset
data <- phylo.pres(ras, tree)

## calculate the metrics
pd <- rast.pd(data$x, tree)
pd.f <- rast.pd(data$x[[1:29]], tree) # for delta grid

pe <- rast.pe(data$x, tree)
pe.f <- rast.pe(data$x[[1:29]], tree) # for delta grid

we <- rast.we(data$x)

## calculate the standard effect size for PD
library(SESraster)
null.pd <- rast.pd.ses(data$x, tree, aleats = 10, random = "spat")

## calculate delta grid for PD and PE
dpd <- delta.grid(pd, pd.f)
dpe <- delta.grid(pe, pe.f)

## figures
# breaks and colors

# PD
breaks_pd <- seq(2.019998, 13.627872, by = 0.01)
colors_pd <- colorRampPalette(c("grey88","#009DC9","#5CFF00","#FDFB00", "orange","#FF0C12"))(length(breaks_pd)-1)

# PD SES
breaks_pdses <- seq(-5.114733, 4.577930, by = 0.01)
colors_pdses <- colorRampPalette(c("grey88","#009DC9","#5CFF00","#FDFB00", "orange","#FF0C12"))(length(breaks_pdses)-1)

# WE
breaks_we <- seq(0.0007231441, 0.2989257023, by = 0.001)
colors_we <- colorRampPalette(c("grey88","#009DC9","#5CFF00","#FDFB00", "orange","#FF0C12"))(length(breaks_we)-1)

# PE
breaks_pe <- seq(0.0002224064, 0.1756404428, by = 0.001)
colors_pe <- colorRampPalette(c("grey88","#009DC9","#5CFF00","#FDFB00", "orange","#FF0C12"))(length(breaks_pe)-1)

# delta PD
breaks_dpd <- seq(-2.364139, 0.000000, by = 0.01)
colors_dpd <- colorRampPalette(c("darkred","#B30D02","red","#C0C6CB"))(length(breaks_dpd))

# delta PE
breaks_dpe <- seq(-0.08144132, 0.01361846, by = 0.01)
colors_dpe <- colorRampPalette(c("#0F0E0F","#450B0C","darkred","#B30D02","red","#C0C6CB","#1034A6"))(length(breaks_dpd))

## plotting figure s1
tiff("figure_S1.tiff", width = 6, height = 6, units = "in",res = 600)
par(mfrow = c(3,2))

plot(pd, col=c(colors_pd), range  = c(2.019998, 13.627872),
     axes = T,
     main = "a) PD",
     plg = list( # parameters for drawing legend
       # title = "a)",
       # title.cex = 2, # Legend title size
       cex = 2), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.2), # Axis text size
     cex.main = 2) # Title text size

plot(null.pd$SES.PD, col = c(colors_pdses),
     range = c(-5.114733, 4.577930),
     axes = T,
     main = "b) SES PD",
     plg = list( # parameters for drawing legend
       # title = "c)",
       # title.cex = 2, # Legend title size
       cex = 2), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.2), # Axis text size
     cex.main = 2) # Title text size

plot(we, col=c(colors_we), range  = c(0.0007231441, 0.2989257023),
     axes = T,
     main = "c) WE",
     plg = list( # parameters for drawing legend
       # title = "a)",
       # title.cex = 2, # Legend title size
       cex = 2), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.2), # Axis text size
     cex.main = 2) # Title text size

plot(pe, col=c(colors_pe), range  = c(0.0002224064, 0.1756404428),
     axes = T,
     main = "d) PE",
     plg = list( # parameters for drawing legend
       # title = "a)",
       # title.cex = 2, # Legend title size
       cex = 2), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.2), # Axis text size
     cex.main = 2) # Title text size

plot(dpd, col = c(colors_dpd),
     range = c(-2.364139 , 0.000000),
     axes = T,
     main = "e) Delta PD",
     plg = list( # parameters for drawing legend
       # title = "c)",
       # title.cex = 2, # Legend title size
       cex = 2), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.2), # Axis text size
     cex.main = 2) # Title text size

plot(dpe, col = c(colors_dpe),
     range = c(-0.08144132, 0.01361846),
     axes = T,
     main = "f) Delta PE",
     plg = list( # parameters for drawing legend
       # title = "c)",
       # title.cex = 2, # Legend title size
       cex = 2), # Legend text size
     pax=list( # parameters for drawing axes
       cex.axis = 1.2), # Axis text size
     cex.main = 2) # Title text size

dev.off()
