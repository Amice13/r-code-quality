################################
## Set path and Load Libraries #
################################

# Please set appropriate path

path <- "C:/Users/adasg/Dropbox/Dust_Bowl/conditional_accept/replication_materials/replication_data/" # your path to directory here

# Loading necessary libraries 
library(rgdal)          # For spatial data operations
library("DiagrammeR")   # For creating diagrams and flowcharts
library(lfe)            # For linear fixed-effects models
library(stargazer)      # For creating well-formatted regression tables
library("DiagrammeRsvg")# For SVG support in DiagrammeR
library(rsvg)           # For handling SVG images
library(magick)         # Image processing and conversion
library(raster)		# For raster operations
library(rgeos)		# For spatial computation
library(rgdal)		# For loading shapefiles
library(geosphere)	# For spatial computation
library(RColorBrewer)	# For color schemes
library(maptools)		# For spatial operations

##############
## Figure 1 ##
##############

# Load necessary files
counties30 <- rgdal::readOGR(paste0(path,"input_files/nhgis0001_shape/nhgis0001_shapefile_tl2008_us_county_1930/US_county_1930_conflated.shp"))
counties90 <- rgdal::readOGR(paste0(path,"input_files/nhgis0001_shape/nhgis0001_shapefile_tl2008_us_county_1990/US_county_1990_conflated.shp"))
load(paste0(path,"output_files/pres_data.Rdata"))
load(paste0(path,"output_files/aquifer.Rdata"))
load(paste0(path,"output_files/states.Rdata"))

# Process data

counties30 <- counties30[counties30@data$STATENAM%in%c("Colorado","North Dakota",
	"South Dakota", "Wyoming", "Nebraska", "Montana", "Oklahoma",
		"Kansas", "New Mexico", "Texas"),]
counties90 <- counties90[counties90@data$STATENAM%in%c("Colorado","North Dakota",
	"South Dakota", "Wyoming", "Nebraska", "Montana", "Oklahoma",
		"Kansas", "New Mexico", "Texas"),]

## Panel A

counties30@data$fips <- paste0(substr(counties30@data$NHGISST, 1, 2),    ## Assign each county a fips code
	substr(counties30@data$NHGISCTY, 1, 3))

for (i in 1:nrow(counties30)){
	republican_votes <- pres_data$republican_raw_votes[pres_data$fips.x==counties30$fips[i] & pres_data$election_year%in%c(1920:1940)]
	total_votes <- pres_data$pres_raw_county_vote_totals_two_party[pres_data$fips.x==counties30$fips[i] & pres_data$election_year%in%c(1920:1940)]
	conservative <- mean(republican_votes/total_votes, na.rm=T)
	counties30$conservative[i] <- ifelse(length(conservative)==0, NA, conservative)
print(i)
}

counties30 <- spTransform(counties30, CRS("+init=epsg:4326"))

## Panel B

counties90@data$fips <- paste0(substr(counties90@data$NHGISST, 1, 2),    ## Assign each county a fips code
	substr(counties90@data$NHGISCTY, 1, 3))

for (i in 1:nrow(counties90)){
	republican_votes <- pres_data$republican_raw_votes[pres_data$fips.x==counties90$fips[i] & pres_data$election_year%in%c(1980:2000)]
	total_votes <- pres_data$pres_raw_county_vote_totals_two_party[pres_data$fips.x==counties90$fips[i] & pres_data$election_year%in%c(1980:2000)]
	conservative <- mean(republican_votes/total_votes, na.rm=T)
	counties90$conservative[i] <- ifelse(length(conservative)==0, NA, conservative)
print(i)
}

counties90 <- spTransform(counties90, CRS("+init=epsg:4326"))

# Setting up the JPEG output for the plot

jpeg(file=paste0(path,"output_figures/fig1.jpg"), width=1400, height=1050)

# Set up layout for two plots side by side
par(mfrow=c(1,2))

# Panel A: Plotting 1930 county data with color-coding based on 'conservative' values
# Panel B: Similarly plotting for 1990

# Panel A
plot(counties30, col=ifelse(is.na(counties30@data$conservative),"white",
	ifelse(counties30@data$conservative <= .2, "darkblue",
			ifelse(counties30@data$conservative > .2 & counties30@data$conservative <= .4, "blue",
				ifelse(counties30@data$conservative > .4 & counties30@data$conservative <= .6, "purple",
	ifelse(counties30@data$conservative >= .6 & counties30@data$conservative <= .8, "red",
			ifelse(counties30@data$conservative >= .8, "darkred", "white")))))), 
				main="Republican Vote Share, 1920-1940")
plot(states, border="black", lwd=3, add=T)

# Panel B
plot(counties90, col=ifelse(is.na(counties90@data$conservative),"white",
	ifelse(counties90@data$conservative <= .2, "darkblue",
			ifelse(counties90@data$conservative > .2 & counties90@data$conservative <= .4, "blue",
				ifelse(counties90@data$conservative > .4 & counties90@data$conservative <= .6, "purple",
	ifelse(counties90@data$conservative >= .6 & counties90@data$conservative <= .8, "red",
			ifelse(counties90@data$conservative >= .8, "darkred", "white")))))), 
				main="Republican Vote Share, 1980-2000")
plot(states, border="black", lwd=3, add=T)

# Reset layout for the legend
par(mfrow=c(1,1), new=TRUE)
plot.new()

# Add a single legend for both plots
legend("bottom", inset=c(0,0), c("Less than 20 percent", "20 to 40 percent", "40 to 60 percent",
	"60 to 80 percent", "More than 80 percent"), fill=c("darkblue","blue","purple","red","darkred"), cex=1.5, horiz=TRUE, xpd=F)

dev.off()

##############################
## Figure 2: Theory diagram ##
##############################

# No data files necessary

# Define a graph in dot language for DiagrammeR

graph <- "digraph {
    graph [layout = dot]
    node [shape = rectangle, style = filled, fillcolor = Linen]
    process1 [label =  'Technological \n Change', fillcolor = Beige]
    process2 [label =  'Capital-intensive Agriculture', fillcolor = Beige]
    process3 [label = 'Selection for \n Scale']
    process4 [label = 'Agglomeration \n Effects']
    process5 [label = 'Agribusiness \n Influence']
    results [label= 'Conservative \n Voting']
    process1 -> process2
    process2 -> process3 -> results
    process2 -> process4 ->  results
    process2 -> process5 -> results
}"

# Dimensions for 600 DPI for a 5x5 inch image
width_pixels <- 600 * 5
height_pixels <- 600 * 5

# Export the graph as SVG and then convert to PNG with high resolution
temp_png_path <- tempfile(fileext = ".png")
grViz(graph) %>%
    export_svg() %>%
    charToRaw() %>%
    rsvg_png(temp_png_path, width = width_pixels, height = height_pixels)

# Convert PNG to JPEG while maintaining the resolution
image_read(temp_png_path) %>%
    image_convert(format = "jpg") %>%
    image_write(paste0(path, "output_figures/fig2.jpg"), density = "600x600")

###############################################
## Figure 3: Center-pivot irrigation diagram ##
###############################################

# This section involves loading spatial data, transforming projections, and calculating densities

## Panel A: Diagram in Latex
##########

# See latex file: neural_network_architecture.tex

## Panel B: Haskell County Predictions
##########

## Load county shapefiles

counties90 <- readOGR(paste0(path,"input_files/nhgis0001_shape/nhgis0001_shapefile_tl2008_us_county_1990/US_county_1990_conflated.shp"))
counties90 <- spTransform(counties90, CRS("+init=epsg:4326"))
counties <- counties90[counties90@data$STATENAM=="Kansas",]

## Load satellite imagery and PLSS estimates of center-pivot irrigation adoption for Kansas

load(paste0(path,"input_files/deep_learning/ndvi_crop.RData"))
load(paste0(path,"input_files/deep_learning/Kansas.RData"))
plss1 <- spTransform(plss1, CRS("+init=epsg:4326"))

## Crop ndvi and plot

locations <- gCentroid(plss1, byid=T)
haskell <- extent(counties[counties@data$NHGISNAM=="Haskell",])
seed <- which(locations$x > haskell[1] & locations$x < haskell[2] &
	locations$y > haskell[3] & locations$y < haskell[4])

# Open a new JPEG file
jpeg(file=paste0(path,"output_figures/fig3b.jpg"), width=3600, height=3600, res=600)

# Plot without a legend
plot(ndvi.crop, axes=F, legend=F, col = gray.colors(10, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))
plot(plss1[seed,], add=T, border="lightblue", lwd=2)
text(locations$x[seed], locations$y[seed], round(plss1@data$year16[seed], digits=0), cex=.5)

# Close the JPEG file
dev.off()

## Panel C: Kansas PLSS predictions
##########

plss1@data$color <- ifelse(round(plss1@data$year16)==0, "white",
	ifelse(round(plss1@data$year16)==1, "lightgreen",
	ifelse(round(plss1@data$year16)==2, "darkgreen",
	ifelse(round(plss1@data$year16)==3, "blue",
	ifelse(round(plss1@data$year16)>=4, "darkblue","white")))))

load(paste0(path, "output_files/aquifer.RData"))
state_kansas <- unionSpatialPolygons(counties, counties@data$STATENAM)
state_line <- rgeos::intersect(aquifer, state_kansas) ## get within-state boundary segment

# Open a new JPEG file with 6x6 inch size at 600 DPI
jpeg(file=paste0(path,"output_figures/fig3c.jpg"), width=4200, height=3600, res=600)

# Plotting commands
plot(plss1, col=plss1@data$color, border=F)
legend("bottom", c("0","1","2","3","4+"), fill=c("white","lightgreen","darkgreen","blue","darkblue"), inset=c(0, 0), cex=.8, horiz=TRUE, x.intersp=0.5)
plot(counties, add=T)
plot(state_line, add=T, border="lightblue", lwd=3)

# Close the JPEG file
dev.off()

## Panel D: County-wise predictions
##########

# Compute county-wise density of center-pivot irrigation adoption

plss_centroids <- gCentroid(plss1, byid=T) ## Get centroids of
plss_area <- areaPolygon(plss1, byid=T) ## In mercator projection the unit is meters
PC <- over(plss_centroids, counties) ## Find which county contains the centroid!

for (i in 1:nrow(counties)){
	seed <- which(PC$PID==counties@data$PID[i])
	if(length(seed)>0){
		for (j in 1:16){
		temporary <- na.omit(data.frame(count=plss1@data[seed,paste0("year",j)], area=plss_area[seed]))
		density <- 1000*1000000*(sum(temporary$count)/sum(temporary$area)) ## Number of center pivots per 1000 sq km
		counties[i, paste0("year",j)] <- ifelse(length(density)>0, density, NA)
		}	
	}
print(i)
}

# Open a new JPEG file with 6x6 inch size at 600 DPI
jpeg(file=paste0(path,"output_figures/fig3d.jpg"), width=4200, height=3600, res=600)

mini_dat <- subset(counties@data, select=c("year1","year2","year3",
    "year4","year5","year6","year7","year8","year9",
    "year10","year11","year12","year13","year14",
    "year15","year16"))
var <- apply(mini_dat, 1, mean, na.rm=T)
pal <- brewer.pal(4, "YlGnBu") # Select 4 colors from the palette 
cols <- as.numeric(as.factor(cut(var, quantile(var), include.lowest=T)))
plot(counties, col=pal[cols]) # Plot by quartiles of variable

# Adjust the legend to span the bottom of the image
legend("bottom", inset=c(0,0), fill=pal, c("1st","2nd","3rd","4th"), cex=.8, horiz=TRUE, x.intersp=0.5)

# Additional plots
plot(state_line, add=T, border="lightblue", lwd=3)

# Close the JPEG file
dev.off()

#######################################################
## Figure 4: Illustration of identification strategy ##
#######################################################

# Load files

load(paste0(path,"output_files/county_panel.RData"))
load(paste0(path,"output_files/aquifer.RData"))
load(paste0(path, "output_files/states.RData"))

# Process data

cross_section <- county_panel[county_panel$DECADE==1990,]

jpeg(file=paste0(path,"output_figures/fig4.jpg"), width=1400, height=1200)

par(mfrow=c(1,3))

# Full sample

plot(cross_section[is.na(cross_section@data$dist)==F & cross_section@data$dist < 200,], main="A. Full Sample")
plot(cross_section[!(cross_section@data$STATENAM%in%c("Montana","North Dakota")),], add=T, 
	col=ifelse(cross_section@data$overlap[!(cross_section@data$STATENAM%in%c("Montana","North Dakota"))]==1, "blue",
		ifelse(cross_section@data$overlap[!(cross_section@data$STATENAM%in%c("Montana","North Dakota"))]<1 & cross_section@data$overlap[!(cross_section@data$STATENAM%in%c("Montana","North Dakota"))]>0, "lightblue","lightyellow")))
plot(states, border="black", lwd=3, add=T)
plot(aquifer, add=T, border="red", lwd=2)

# Limited those within 200 m distance to boundary

plot(cross_section[cross_section@data$STATENAM!="Montana" & cross_section@data$dist < 200 & is.na(cross_section@data$dist)==F,], main="B. 200km Sample",
	col=ifelse(cross_section@data$overlap[cross_section@data$STATENAM!="Montana" & cross_section@data$dist < 200 & is.na(cross_section@data$dist)==F]==1, "blue",
		ifelse(cross_section@data$overlap[cross_section@data$STATENAM!="Montana" & cross_section@data$dist < 200 & is.na(cross_section@data$dist)==F]<1 & cross_section@data$overlap[cross_section@data$STATENAM!="Montana" & cross_section@data$dist < 200 & is.na(cross_section@data$dist)==F]>0, "lightblue","lightyellow")))
plot(states, border="black", lwd=3, add=T)
plot(aquifer, add=T, border="red", lwd=2)

# Limited to those within 100 m distance to boundary

plot(cross_section[cross_section@data$STATENAM!="Montana" & cross_section@data$dist < 100 & is.na(cross_section@data$dist)==F,], main="C. 100km Sample",
	col=ifelse(cross_section@data$overlap[cross_section@data$STATENAM!="Montana" & cross_section@data$dist < 100 & is.na(cross_section@data$dist)==F]==1, "blue",
		ifelse(cross_section@data$overlap[cross_section@data$STATENAM!="Montana" & cross_section@data$dist < 100 & is.na(cross_section@data$dist)==F]<1 & 
			cross_section@data$overlap[cross_section@data$STATENAM!="Montana" & cross_section@data$dist < 100 & is.na(cross_section@data$dist)==F]>0, "lightblue","lightyellow")))
plot(states, border="black", lwd=3, add=T)
plot(aquifer, add=T, border="red", lwd=2)

# Reset layout for the legend
par(mfrow=c(1,1), new=TRUE)
plot.new()

# Add a single legend for both plots
legend("bottom", inset=c(0,0), c("Pruned", "No overlap","Partial overlap","Complete overlap"), fill=c("white","lightyellow","lightblue","blue"), cex=2, horiz=TRUE, xpd=F)

dev.off()

#############################################################
## Figure 5: Scatterplot, pre and post technological shock ##
#############################################################

# Load data

load(paste0(path,"output_files/pres_data.Rdata"))
load(paste0(path,"output_files/county_frame.Rdata"))

# Process data

fig5 <- subset(pres_data,pres_data$fips.x%in%county_frame$fips[county_frame$stable==1] & pres_data$dist < 200 & !(pres_data$STATENAM%in%c("Montana","North Dakota")))

# Plot

pdf(file=paste0(path,"output_figures/fig5.pdf"), width=12, height=9)

par(mfrow=c(1,2))

fig5pre <- fig5[fig5$election_year%in%c(1920:1940),]
x <- tapply(fig5pre$overlap, fig5pre$fips.x, mean, na.rm=T)
y <- tapply(fig5pre$republican_raw_votes/fig5pre$pres_raw_county_vote_totals_two_party, fig5pre$fips.x, mean, na.rm=T)
z <- tapply(fig5pre$STATENAM, fig5pre$fips.x, unique)
n <- tapply(fig5pre$NHGISNAM, fig5pre$fips.x, unique)
lm1 <- lm(x ~ as.factor(z))
lm2 <- lm(y ~ as.factor(z))
plot(residuals(lm1), residuals(lm2), main="A. Pre-technological shock (1920-1940)",
	xlab="Aquifer Coverage | State Fixed Effects", ylab="Republican Share of Two-party Vote | State Fixed Effects", pch=1, cex=2, xlim=c(-1,1), ylim=c(-.3, .3))
abline(lm(residuals(lm2) ~ residuals(lm1)))
points(residuals(lm1)[z=="Kansas"], residuals(lm2)[z=="Kansas"], pch=20, cex=3)
points(residuals(lm1)[n=="Haskell" & z=="Kansas"], residuals(lm2)[n=="Haskell"& z=="Kansas"], pch=18, cex=3, col="red")
text(residuals(lm1)[n=="Haskell" & z=="Kansas"]+.2, residuals(lm2)[n=="Haskell"& z=="Kansas"]+.02, "Haskell county",col="red")

fig5post <- fig5[fig5$election_year%in%c(1980:2000),]
x <- tapply(fig5post$overlap, fig5post$fips.x, mean, na.rm=T)
y <- tapply(fig5post$republican_raw_votes/fig5post$pres_raw_county_vote_totals_two_party, fig5post$fips.x, mean, na.rm=T)
z <- tapply(fig5post$STATENAM, fig5post$fips.x, unique)
n <- tapply(fig5post$NHGISNAM, fig5post$fips.x, unique)
lm1 <- lm(x ~ as.factor(z))
lm2 <- lm(y ~ as.factor(z))
plot(residuals(lm1), residuals(lm2), main="B. Post-technological shock (1980-2000)",
	xlab="Aquifer Coverage | State Fixed Effects", ylab="Republican Share of Two-party Vote | State Fixed Effects", pch=21, cex=2.1, xlim=c(-1,1), ylim=c(-.3, .3))
abline(lm(residuals(lm2) ~ residuals(lm1)))
points(residuals(lm1)[z=="Kansas"], residuals(lm2)[z=="Kansas"], pch=20, cex=3)
points(residuals(lm1)[n=="Haskell" & z=="Kansas"], residuals(lm2)[n=="Haskell"& z=="Kansas"], pch=18, cex=3, col="red")
text(residuals(lm1)[n=="Haskell" & z=="Kansas"]+.2, residuals(lm2)[n=="Haskell"& z=="Kansas"]+.02, "Haskell county",col="red")

dev.off()

#############
## TABLE 1 ##
#############

load(paste0(path,"output_files/dat.Rdata"))
load(paste0(path,"output_files/dat200.Rdata"))
load(paste0(path,"output_files/dat100.Rdata"))

## FULL SAMPLE
##############

# Baseline 

lm1 <- felm(pres ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat)

lm2 <- felm(sen ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat)

lm3 <- felm(gov ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat)

# Reweighted OLS 

lm1_reweight <- felm(pres ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=1/dat$w, data=dat)

lm2_reweight <- felm(sen ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=1/dat$w, data=dat)

lm3_reweight <- felm(gov ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=1/dat$w, data=dat)

# Time-interacted controls

lm4 <- felm(pres ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post) + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=dat)

lm5 <- felm(sen ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=dat)

lm6 <- felm(gov ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=dat)

# Pure tx/control

lm7 <- felm(pres ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[(dat$overlap %in% c(0,1)) ,])

lm8 <- felm(sen ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[(dat$overlap %in% c(0,1)),])

lm9 <- felm(gov ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[(dat$overlap %in% c(0,1)),])

stargazer_output <- stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"), type='text')

write(stargazer_output, file=paste0(path,"output_figures/table1_panelA.txt"))


## 200 km sample
################

# Baseline

lm1 <- felm(pres ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat200)

lm2 <- felm(sen ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat200)

lm3 <- felm(gov ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat200)

# Reweighted OLS 

lm1_reweight <- felm(pres ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=1/dat200$w, data=dat200)

lm2_reweight <- felm(sen ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=1/dat200$w, data=dat200)

lm3_reweight <- felm(gov ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=1/dat200$w, data=dat200)

# Time-interacted controls

lm4 <- felm(pres ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=dat200)

lm5 <- felm(sen ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=dat200)

lm6 <- felm(gov ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=dat200)

# Pure tx/control

lm7 <- felm(pres ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat200[(dat200$overlap %in% c(0,1)),])

lm8 <- felm(sen ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat200[(dat200$overlap %in% c(0,1)),])

lm9 <- felm(gov ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat200[(dat200$overlap %in% c(0,1)),])

stargazer_output <- stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"), type='text')

write(stargazer_output, file=paste0(path,"output_figures/table1_panelB.txt"))


## 100 KM SAMPLE 
#########################

# Baseline

lm1 <- felm(pres ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat100)

lm2 <- felm(sen ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat100)

lm3 <- felm(gov ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat100)

# Reweighted OLS 

lm1_reweight <- felm(pres ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=1/dat100$w, data=dat100)

lm2_reweight <- felm(sen ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=1/dat100$w, data=dat100)

lm3_reweight <- felm(gov ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=1/dat100$w, data=dat100)

# Time-interacted controls

lm4 <- felm(pres ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=dat100)

lm5 <- felm(sen ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=dat100)

lm6 <- felm(gov ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=dat100)

# Pure tx/control

lm7 <- felm(pres ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat100[(dat100$overlap %in% c(0,1)),])

lm8 <- felm(sen ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat100[(dat100$overlap %in% c(0,1)),])

lm9 <- felm(gov ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat100[(dat100$overlap %in% c(0,1)),])

stargazer_output <- stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"), type='text')

write(stargazer_output, file=paste0(path,"output_figures/table1_panelC.txt"))


#############
## TABLE 2 ##
#############

load(paste0(path,"output_files/dat.Rdata"))
load(paste0(path,"output_files/dat200.Rdata"))
load(paste0(path,"output_files/dat100.Rdata"))

lm1 <- felm(irr2 ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat)

lm2 <- felm(irr2 ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[dat$dist <= 200,])

lm3 <- felm(irr2 ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[dat$dist <= 100,])

lm4 <- felm(center_pivot ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat)

lm5 <- felm(center_pivot ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[dat$dist <= 200,])

lm6 <- felm(center_pivot ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[dat$dist <= 100,])

stargazer_output <- stargazer(lm1,lm2,lm3,lm4,lm5,lm6, type='text')

write(stargazer_output, file=paste0(path,"output_figures/table2.txt"))


#############
## TABLE 3 ##
#############

load(paste0(path,"output_files/pres_data.Rdata"))
load(paste0(path,"output_files/sen_data.Rdata"))
load(paste0(path,"output_files/gov_data.Rdata"))
load(paste0(path,"output_files/agdat.Rdata"))
load(paste0(path,"output_files/county_frame.Rdata"))

# President full sample

tab3dat1 <- pres_data[is.na(pres_data$dist)==F & pres_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm1 <- felm(conservative ~ 
	I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=tab3dat1)

## President 200km

tab3dat2 <- pres_data[is.na(pres_data$dist)==F & pres_data$dist < 200 & pres_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm2 <- felm(conservative ~ 
	I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=tab3dat2)

## President 100km

tab3dat3 <- pres_data[is.na(pres_data$dist)==F & pres_data$dist < 100 & pres_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm3 <- felm(conservative ~ 
	I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=tab3dat3)
## Senator Full

tab3dat4 <- sen_data[is.na(sen_data$dist)==F & sen_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm4 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=tab3dat4)

## Senator 200km

tab3dat5 <- sen_data[is.na(sen_data$dist)==F & sen_data$dist < 200 & sen_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm5 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=tab3dat5)

## Senator 100km

tab3dat6 <- sen_data[is.na(sen_data$dist)==F & sen_data$dist < 100 & sen_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm6 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=tab3dat6)

## Governor Full

tab3dat7 <- gov_data[is.na(gov_data$dist)==F & gov_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm7 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=tab3dat7)

## Governor 200km

tab3dat8 <- gov_data[is.na(gov_data$dist)==F & gov_data$dist < 200 & gov_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm8 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=tab3dat8)

## Governor 100km

tab3dat9 <- gov_data[is.na(gov_data$dist)==F & gov_data$dist < 100 & gov_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm9 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=tab3dat9)

## Irrigation Full

lm10 <- felm(irr2 ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, 
			data=agdat[is.na(agdat$dist)==F  & agdat$fips%in%county_frame$fips[county_frame$stable],])

## Irrigation 200km

lm11 <- felm(irr2 ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, 
			data=agdat[agdat$dist < 200  & agdat$fips%in%county_frame$fips[county_frame$stable],])

## Irrigation 100km

lm12 <- felm(irr2 ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, 
			data=agdat[is.na(agdat$dist)==F & agdat$dist < 100  & agdat$fips%in%county_frame$fips[county_frame$stable],])

stargazer_output <- stargazer(lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9, lm10, lm11, lm12, type='text')

write(stargazer_output, file=paste0(path,"output_figures/table3.txt"))

#################################################
## Figure 6: Results broken out by time period ##
#################################################

load(paste0(path,"output_files/pres_data.Rdata"))
load(paste0(path,"output_files/sen_data.Rdata"))
load(paste0(path,"output_files/gov_data.Rdata"))
load(paste0(path,"output_files/agdat.Rdata"))
load(paste0(path,"output_files/county_frame.Rdata"))

pdf(file=paste0(path,"output_figures/fig6.pdf"), width=9, height=9)

par(mfrow=c(2, 2))

## President

tab3dat3 <- pres_data[is.na(pres_data$dist)==F & pres_data$dist < 100 & pres_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm1 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id + fips.x | 0 | election_id + fips.x, 
			data=tab3dat3)
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]), ylim=c(-sd(pres_data$conservative, na.rm=T),sd(pres_data$conservative, na.rm=T)), main="A. Presidential Elections",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s")
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

## Senate

tab3dat6 <- sen_data[is.na(sen_data$dist)==F & sen_data$dist < 100 & sen_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm1 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id + fips.x | 0 | election_id + fips.x, 
			data=tab3dat6)
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]), ylim=c(-sd(sen_data$conservative, na.rm=T),sd(sen_data$conservative, na.rm=T)), main="B. Senatorial Elections",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s")
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

## Governor

tab3dat9 <- gov_data[is.na(gov_data$dist)==F & gov_data$dist < 100 & gov_data$fips.x%in%county_frame$fips[county_frame$stable],]

lm1 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id + fips.x | 0 | election_id + fips.x, 
			data=tab3dat9)
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]), ylim=c(-sd(gov_data$conservative, na.rm=T),sd(gov_data$conservative, na.rm=T)), main="C. Gubernatorial Elections",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s")
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

## Irrigation 

lm1 <- felm(irr2  ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear  + fips | 0 | stateyear + fips, 
					data=agdat[is.na(agdat$dist)==F & agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="D. Farmland Irrigated (%)",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-2*sd(agdat$irr2, na.rm=T),2*sd(agdat$irr2, na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

dev.off()

#################################
## Table 4: Potential Channels ##
#################################

# Load files

load(paste0(path,"output_files/agdat.Rdata"))
load(paste0(path,"output_files/popdat.Rdata"))
load(paste0(path,"output_files/religion_dat.Rdata"))
load(paste0(path,"output_files/county_frame.Rdata"))
load(paste0(path,"output_files/race.Rdata"))

## Machinery

lm1 <- felm(machinery_per_farm ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, 
					data=agdat[agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])

## Market Value of Land and buildings

lm2 <- felm(log(value_per_farm) ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, 
					data=agdat[agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])

## Density of farms

lm3 <- felm(farms_per_acre ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, 
					data=agdat[agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])

## Crop Output

lm4 <- felm(crop_output_per_farm ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, 
					data=agdat[agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])

## Number of livestock per farm

lm5 <- felm(livestock_per_farm ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, 
					data=agdat[agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])

## Agricultural sector employment

lm6 <- felm(ag_percent ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920))+ I(overlap*I(buckets==1930)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, data=popdat[popdat$dist <100 & popdat$YEAR >= 1910 & 
				popdat$fips%in%county_frame$fips[county_frame$stable],])

# Population density

lm7 <- felm(I(1000*pop_density) ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, data=popdat[popdat$dist <100 & popdat$YEAR >= 1910 & 
				popdat$fips%in%county_frame$fips[county_frame$stable],])

# Urbanization rate

lm8 <- felm(urban_percent ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920))+ I(overlap*I(buckets==1930)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, data=popdat[popdat$dist <100 & popdat$YEAR >= 1910 & 
				popdat$fips%in%county_frame$fips[county_frame$stable],])
# Religosity

lm9 <- felm(membership ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920))+ I(overlap*I(buckets==1930)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, data=religion_dat[religion_dat$dist <100 & religion_dat$YEAR >= 1910  & religion_dat$fips%in%county_frame$fips[county_frame$stable],])
# Racial composition

lm10 <- felm(white~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960)) + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear | 0 | stateyear + fips, data=race[race$dist <100 & race$YEAR >= 1910 & race$fips%in%county_frame$fips[county_frame$stable],])

stargazer_output <- stargazer(lm1,lm2,lm3,lm6,lm4,lm5,lm7,lm8,lm9,lm10, type='text')

write(stargazer_output, file=paste0(path,"output_figures/table4.txt"))

##################################
## Figure 7: Potential Channels ##
##################################

load(paste0(path,"output_files/agdat.Rdata"))
load(paste0(path,"output_files/popdat.Rdata"))
load(paste0(path,"output_files/religion_dat.Rdata"))
load(paste0(path,"output_files/county_frame.Rdata"))
load(paste0(path,"output_files/race.Rdata"))

pdf(file=paste0(path,"output_figures/fig7.pdf"), width=12, height=8)

par(mfrow=c(2,5))

agdat$x <- agdat$machinery_per_farm
lm1 <- felm(x ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear + fips| 0 | stateyear + fips, 
					data=agdat[agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="A. Machinery per Farm $",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-2*sd(agdat$x,na.rm=T),2*sd(agdat$x, na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

agdat$x <- log(agdat$value_per_farm)
lm1 <- felm(x ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear + fips| 0 | stateyear + fips, 
					data=agdat[agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="B. Log Per Farm Value",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-sd(agdat$x[!(agdat$x%in%c(-Inf,Inf))], na.rm=T),sd(agdat$x[!(agdat$x%in%c(-Inf,Inf))], na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

agdat$x <- agdat$farms_per_acre
lm1 <- felm(x ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear + fips| 0 | stateyear + fips, 
					data=agdat[agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="C. Farm Density",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-sd(agdat$x[!(agdat$x%in%c(-Inf,Inf))], na.rm=T),sd(agdat$x[!(agdat$x%in%c(-Inf,Inf))], na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

lm1 <- felm(log(ag_percent+1) ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear + fips| 0 | stateyear + fips, data=popdat[popdat$dist <100 & popdat$YEAR >= 1910 & 
				popdat$fips%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="D. Log Agricultural Employment",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-sd(log(popdat$ag_percent+1), na.rm=T),sd(log(popdat$ag_percent+1), na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

lm1 <- felm(crop_output_per_farm ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear + fips| 0 | stateyear + fips, 
					data=agdat[agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="E. Crop Productivity",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-sd(agdat$crop_output_per_farm[!(agdat$crop_output_per_farm%in%c(-Inf,Inf))], na.rm=T),sd(agdat$crop_output_per_farm[!(agdat$crop_output_per_farm%in%c(-Inf,Inf))], na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

lm1 <- felm(livestock_per_farm ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear + fips| 0 | stateyear + fips, 
					data=agdat[agdat$dist < 100 & agdat$fips%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="F. Livestock",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-sd(agdat$livestock_per_farm[!(agdat$livestock_per_farm%in%c(-Inf,Inf))], na.rm=T),sd(agdat$livestock_per_farm[!(agdat$livestock_per_farm%in%c(-Inf,Inf))], na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)


lm1 <- felm(I(1000*pop_density)~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear + fips| 0 | stateyear + fips, data=popdat[popdat$dist <100 & popdat$YEAR >= 1910 & 
				popdat$fips%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="G. Population Density",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-sd(1000*popdat$pop_density, na.rm=T),sd(1000*popdat$pop_density, na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

lm1 <- felm(urban_percent ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear + fips| 0 | stateyear + fips, data=popdat[popdat$dist <100 & popdat$YEAR >= 1910 & 
				popdat$fips%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="H. Urbanization",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-sd(popdat$urban_percent, na.rm=T),sd(popdat$urban_percent, na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

lm1 <- felm(membership~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960)) + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear + fips| 0 | stateyear + fips, data=religion_dat[religion_dat$dist <100 & religion_dat$YEAR >= 1910 &  religion_dat$fips%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="I. Church Membership",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-sd(religion_dat$membership, na.rm=T),sd(religion_dat$membership, na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

lm1 <- felm(white~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960)) + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | stateyear + fips| 0 | stateyear + fips, data=race[race$dist <100 & race$YEAR >= 1910 & race$fips%in%county_frame$fips[county_frame$stable],])

plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), 
	c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]),  main="J. Race",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s", ylim=c(-sd(race$white, na.rm=T),sd(race$white, na.rm=T)))
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

dev.off()

################################
## Figure 8: CCES Survey Data ##
################################

# Load files

load(paste0(path, "output_files/zip_zample.RData"))
load(paste0(path,"output_files/aquifer.RData"))
load(paste0(path, "output_files/states.RData"))

# Produce plot

zip_sample@data$aquifer <- round(zip_sample@data$aquifer, digits=3)

jpeg(file=paste0(path,"output_figures/fig8.jpg"), width=1400, height=1200)

par(mfrow=c(1,3))

plot(zip_sample[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F,],  
	col=ifelse(zip_sample@data$aquifer[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F]==1,"blue",
		ifelse(zip_sample@data$aquifer[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F]>0, "lightblue", "lightyellow")),
	main="A. Full Sample")
plot(states, border="black", lwd=3, add=T)
plot(aquifer, add=T, border="red",lwd=2)

plot(zip_sample[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F,],  
	col="white", border="white", main="B. 200km Buffer")
plot(states,border="black", lwd=3, add=T)
plot(zip_sample[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F & zip_sample@data$dist < 200,], add=T, 
	col=ifelse(zip_sample@data$aquifer[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F & zip_sample@data$dist < 200]==1,"blue",
		ifelse(zip_sample@data$aquifer[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F & zip_sample@data$dist < 200]>0, "lightblue", "lightyellow")))
plot(aquifer, add=T, border="red",lwd=2)

plot(zip_sample[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F,],  
	col="white", border="white", main="C. 100km Buffer")
plot(states,border="black", lwd=3, add=T)
plot(zip_sample[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F & zip_sample@data$dist < 100,], add=T, 
	col=ifelse(zip_sample@data$aquifer[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F & zip_sample@data$dist < 100]==1,"blue",
		ifelse(zip_sample@data$aquifer[is.na(zip_sample@data$in_state)==F & is.na(zip_sample@data$dist)==F & zip_sample@data$dist < 100]>0, "lightblue", "lightyellow")))
plot(aquifer, add=T, border="red",lwd=2)

# Reset layout for the legend
par(mfrow=c(1,1), new=TRUE)
plot.new()

# Add a single legend for both plots
legend("bottom", inset=c(0,0), c("Pruned", "No overlap","Partial overlap","Complete overlap"), fill=c("white","lightyellow","lightblue","blue"), cex=2, horiz=TRUE, xpd=F)

dev.off()

## Table 5: CCES results
########################

# Load files

load(paste0(path,"output_files/tab5.Rdata"))

## Do some dataset processing

temp3 <- subset(temp3, is.na(temp3$in_state)==F)
temp3$stateyear <- paste(temp3$in_state, temp3$year)

temp3$spending_vs_tax <- ifelse(temp3$spending_vs_tax%in%c(0:100),temp3$spending_vs_tax, NA)
temp3$welfare_decrease <- ifelse(temp3$spending_welfare==4 | temp3$spending_welfare==5,1, 0)
temp3$zipcode_numeric <- as.numeric(temp3$zipcode)
temp3$year_numeric <- as.numeric(temp3$year)
temp3 <- subset(temp3, is.na(temp3$dist.y)==F)

## Panel 1: Full Sample
##########

# Regulation

lm1a <- felm(enviro_airwateracts==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3, ## Full sample
	weights=temp3$weight_cumulative)
lm1b <- felm(repealaca==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3, ## Full sample
	weights=temp3$weight_cumulative)
lm1c <- felm(immig_employer==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3, ## Full sample
	weights=temp3$weight_cumulative)

# Tax and Spending

lm1d <- felm(immig_services==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3, ## Full sample
	weights=temp3$weight_cumulative)
lm1e <- felm(spending_vs_tax ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3, ## Full sample
	weights=temp3$weight_cumulative)
lm1f <- felm(welfare_decrease ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3, ## Full sample
	weights=temp3$weight_cumulative)

# Cultural issues

lm1h <- felm(abortion_always==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3, ## Full sample
	weights=temp3$weight_cumulative)
lm1i <- felm(guns_bgchecks==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3, ## Full sample
	weights=temp3$weight_cumulative)
lm1j <- felm(I(affirmativeaction%in%c(3,4)) ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3, ## Full sample
	weights=temp3$weight_cumulative)

stargazer_output <- stargazer(lm1a,lm1b,lm1c,lm1d,lm1e,lm1f,lm1h, lm1i, lm1j, type='text')
write(stargazer_output, file=paste0(path,"output_figures/table5_panelA.txt"))


## Panel 2: 200km sample
##########

# Regulation

lm1a <- felm(enviro_airwateracts==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 200,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 200])
lm1b <- felm(repealaca==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 200,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 200])
lm1c <- felm(immig_employer==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 200,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 200])

# Tax and Spending

lm1d <- felm(immig_services==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 200,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 200])
lm1e <- felm(spending_vs_tax ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 200,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 200])
lm1f <- felm(welfare_decrease ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 200,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 200])

# Cultural issues

lm1h <- felm(abortion_always==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 200,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 200])
lm1i <- felm(guns_bgchecks==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 200,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 200])
lm1j <- felm(I(affirmativeaction%in%c(3,4)) ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 200,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 200])

stargazer_output <- stargazer(lm1a,lm1b,lm1c,lm1d,lm1e,lm1f,lm1h, lm1i, lm1j, type='text')
write(stargazer_output, file=paste0(path,"output_figures/table5_panelB.txt"))

## Panel C: 100km sample
##########

# Regulation

lm1a <- felm(enviro_airwateracts==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100])
lm1b <- felm(repealaca==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100])
lm1c <- felm(immig_employer==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100])

# Tax and Spending

lm1d <- felm(immig_services==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100])
lm1e <- felm(spending_vs_tax ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100])
lm1f <- felm(welfare_decrease ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100])

# Cultural issues

lm1h <- felm(abortion_always==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100])
lm1i <- felm(guns_bgchecks==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100])
lm1j <- felm(I(affirmativeaction%in%c(3,4)) ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100,], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100])

stargazer_output <- stargazer(lm1a,lm1b,lm1c,lm1d,lm1e,lm1f,lm1h, lm1i, lm1j, type='text')
write(stargazer_output, file=paste0(path,"output_figures/table5_panelC.txt"))


## Panel D: 100km sample + pruning zipcodes on the boundary
##########

# Regulation

lm1a <- felm(enviro_airwateracts==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100 & temp3$aquifer%in%c(0,1),], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100& temp3$aquifer%in%c(0,1)])
lm1b <- felm(repealaca==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100& temp3$aquifer%in%c(0,1),], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100& temp3$aquifer%in%c(0,1)])
lm1c <- felm(immig_employer==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100& temp3$aquifer%in%c(0,1),], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100& temp3$aquifer%in%c(0,1)])

# Tax and Spending

lm1d <- felm(immig_services==1 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100& temp3$aquifer%in%c(0,1),], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100& temp3$aquifer%in%c(0,1)])
lm1e <- felm(spending_vs_tax ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100& temp3$aquifer%in%c(0,1),], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100& temp3$aquifer%in%c(0,1)])
lm1f <- felm(welfare_decrease ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100& temp3$aquifer%in%c(0,1),], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100& temp3$aquifer%in%c(0,1)])

# Cultural issues

lm1h <- felm(abortion_always==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100& temp3$aquifer%in%c(0,1),], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100& temp3$aquifer%in%c(0,1)])
lm1i <- felm(guns_bgchecks==2 ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100& temp3$aquifer%in%c(0,1),], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100& temp3$aquifer%in%c(0,1)])
lm1j <- felm(I(affirmativeaction%in%c(3,4)) ~ aquifer | stateyear | 0 | county_fips + stateyear, data=temp3[temp3$dist.y < 100& temp3$aquifer%in%c(0,1),], ## Full sample
	weights=temp3$weight_cumulative[temp3$dist.y < 100& temp3$aquifer%in%c(0,1)])

stargazer_output <- stargazer(lm1a,lm1b,lm1c,lm1d,lm1e,lm1f,lm1h, lm1i, lm1j, type='text')
write(stargazer_output, file=paste0(path,"output_figures/table5_panelD.txt"))

#####################
## ONLINE APPENDIX ##
#####################

## FIGURE A1-A5 
###############

# Not based on statistical analysis

## FIGURE A6: Scatter plot of Accuracy of computer vision estimates
###################################################

# Load Nebraska center-pivot estimates by section

load(paste0(path,"input_files/deep_learning/Nebraska.RData")) 
plss1 <- spTransform(plss1, CRS("+init=epsg:4326"))
plss_centroids <- gCentroid(plss1, byid=T) ## Get centroids of
plss_area <- areaPolygon(plss1, byid=T) ## In mercator projection the unit is meters

# Load Nebraska shapefiles 

counties <- readOGR(paste0(path,"input_files/nhgis0001_shape/nhgis0001_shapefile_tl2008_us_county_1990/US_county_1990_conflated.shp"))
counties <- counties[counties@data$STATENAM=="Nebraska",]
counties <- spTransform(counties, CRS("+init=epsg:4326"))
PC <- over(plss_centroids, counties)

# Compare machine learning predictions with ground truth in test set

frame <- data.frame(year=c(rep(1985, times=nrow(counties)),rep(1986, times=nrow(counties)),rep(1987, times=nrow(counties))),
	county=c(1:nrow(counties),1:nrow(counties),1:nrow(counties)))
set.seed(94610)
n <- sample(1:nrow(frame), round(0.8*nrow(frame)), replace=F)
train_frame <- frame[n,] # Reconstruct stratified division of observations by county-year in Nebraska
test_frame <- frame[-n,]

filepath <- paste0(paste0(path,"input_files/deep_learning/test"))
files <- list.files(filepath)

for (i in 1:nrow(test_frame)){
	seed <- test_frame$county[i] # Get file names
	year <- test_frame$year[i]
	id <- which(PC$PID==counties@data$PID[seed])
	polygons <- (plss1$FRSTDIVID)[id] ## Vector of PLSS section names in given county and year
	
	sections <- intersect(paste0(year,polygons), substr(files,1,24)) 
	file_no <- which(substr(files,1,24)%in%sections) # Get image file names, which include ground truth 
	row_no <- which(paste0(year,plss1$FRSTDIVID)%in%sections) # Get row numbers with predictions corresponding to images

	year_call <- ifelse(year==1985, "year1", 
		ifelse(year==1986, "year2", "year3"))

	test_frame$true_count[i] <- sum(as.numeric(substr(files[file_no], 26, 28))) # ground truth total	
	test_frame$true_density[i] <- 1000*1000000*((test_frame$true_count[i])/sum(plss_area[row_no])) # ground truth density per 1000 sq km

	test_frame$estimated_count[i] <- sum(plss1@data[row_no,year_call], na.rm=T) # predicted total
	test_frame$estimated_density[i] <- 1000*1000000*(test_frame$estimated_count[i]/sum(plss_area[row_no])) # predicted density per 1000 sq km

print(i)
}

## Predictive accuracy plot

par(mfrow=c(1,2))

plot(test_frame$estimated_density, test_frame$true_density, ylim=c(0,500), xlim=c(0,500),
	ylab="Ground-truth Center Pivots per 1000 sq km", xlab="Predicted Center Pivots per 1000 sq km", main="A. Center Pivot Density")
abline(lm(test_frame$true_density ~ test_frame$estimated_density), lty=1, col="red")
points(seq(from=0,to=5000, by=5000), seq(from=0,to=5000, by=5000), type='l', col="blue", lty=2)

plot(test_frame$estimated_count, test_frame$true_count, ylim=c(0,2000), xlim=c(0,2000),
	ylab="Ground-truth Center Pivot Count", xlab="Predicted Center Pivot Count", main="B. Center Pivot Count")
abline(lm(test_frame$true_count ~ test_frame$estimated_count), lty=1, col="red")
points(seq(from=0,to=5000, by=5000), seq(from=0,to=5000, by=5000), type='l', col="blue", lty=2)

## Figure A7: Map of Great Plains counties shaded by center-pivot irrigation quartile
####################################################

load(paste0(path,"input_files/deep_learning/big_map2022-09-12.RData"))

mini_dat <- subset(big_map@data, select=c("year1","year2","year3",
    "year4","year5","year6","year7","year8","year9",
    "year10","year11","year12","year13","year14",
    "year15","year16"))
var <- apply(mini_dat, 1, mean, na.rm=T)
pal <- brewer.pal(4, "YlGnBu") # Select 4 colors from the palette 
cols <- as.numeric(as.factor(cut(var, quantile(var), include.lowest=T)))

plot(big_map, col=pal[cols]) # Plot by quartiles of variable
plot(aquifer, add=T, border="red")

# Adjust the legend to span the bottom of the image
legend("bottom", inset=c(0,0), fill=pal, c("1st","2nd","3rd","4th"), cex=.8, horiz=TRUE, x.intersp=0.5)

## Figure A8: Soil Conservation Service Map
################################################

# Not based on statistical analysis

## Table A1: Descriptive statistics
###################################

load(paste0(path,"output_files/pres_data.Rdata"))
load(paste0(path,"output_files/sen_data.Rdata"))
load(paste0(path,"output_files/gov_data.Rdata"))
load(paste0(path,"output_files/agdat.Rdata"))
load(paste0(path,"output_files/popdat.Rdata"))
load(paste0(path,"output_files/religion_dat.Rdata"))
load(paste0(path,"output_files/county_frame.Rdata"))
load(paste0(path,"output_files/race.Rdata"))

descriptor <- function(x, var){
	c(tapply(x[,var],x[,"buckets"], mean, na.rm=T),
	tapply(x[x$dist < 200,var],x[x$dist < 200,"buckets"], mean, na.rm=T),
	tapply(x[x$dist < 100,var],x[x$dist < 100,"buckets"], mean, na.rm=T))
}

stable_counties <- county_frame$fips[county_frame$stable ]
GP_states <- c("Oklahoma", "Colorado", "Nebraska", "Wyoming", 
	"Kansas", "Texas", "South Dakota","New Mexico") 

descriptor(pres_data[pres_data$fips.x%in%stable_counties & pres_data$STATENAM%in%GP_states,], "conservative")
descriptor(sen_data[sen_data$fips.x%in%stable_counties & sen_data$STATENAM%in%GP_states,], "conservative")
descriptor(gov_data[gov_data$fips.x%in%stable_counties & gov_data$STATENAM%in%GP_states,], "conservative")
descriptor(agdat[agdat$fips%in%stable_counties & agdat$STATENAM.x%in%GP_states,], "irr2")
descriptor(agdat[agdat$fips%in%stable_counties & agdat$STATENAM.x%in%GP_states,], "value_per_farm")
descriptor(agdat[agdat$fips%in%stable_counties & agdat$STATENAM.x%in%GP_states,], "farms_per_acre")
descriptor(popdat[popdat$fips%in%stable_counties & popdat$STATENAM%in%GP_states,], "ag_percent")
descriptor(popdat[popdat$fips%in%stable_counties & popdat$STATENAM%in%GP_states,], "pop_density")*1000
descriptor(religion_dat[religion_dat$fips%in%stable_counties & religion_dat$STATENAM%in%GP_states,], "membership")

## Table A2: Main results full table (full sample)
####################################

# code produced above


## Table A3: Main results full table (200 km sample)
####################################

# code produced above


## Table A4: Main results full table (100 km sample)
####################################

# code produced above


## Table A5. Pre- and Post-technological Shock Trends
##################################################

# code produced above


## Table A6: Potential Channels (Table Version of Figure 7 inMain Paper)
######################################################

# code produced above 


## Table A7: Impact of Technological Shock on Conservative Voting (County Fixed Effects)
#####################################################

load(paste0(path,"output_files/dat.Rdata"))
load(paste0(path,"output_files/dat200.Rdata"))
load(paste0(path,"output_files/dat100.Rdata"))

## FULL SAMPLE
##############

# Baseline 

lm1 <- felm(pres ~ overlap + I(overlap*post) | stateyear + ID | 0 | ID + stateyear, data=dat)

lm2 <- felm(sen ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat)

lm3 <- felm(gov ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat)

# Reweighted OLS 

lm1_reweight <- felm(pres ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, weights=1/dat$w, data=dat)

lm2_reweight <- felm(sen ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, weights=1/dat$w, data=dat)

lm3_reweight <- felm(gov ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, weights=1/dat$w, data=dat)

# Time-interacted controls

lm4 <- felm(pres ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post) + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear + ID| 0 | ID + stateyear, data=dat)

lm5 <- felm(sen ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear + ID| 0 | ID + stateyear, data=dat)

lm6 <- felm(gov ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear + ID| 0 | ID + stateyear, data=dat)

# Pure tx/control

lm7 <- felm(pres ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat[(dat$overlap %in% c(0,1)) ,])

lm8 <- felm(sen ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat[(dat$overlap %in% c(0,1)),])

lm9 <- felm(gov ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat[(dat$overlap %in% c(0,1)),])

stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"))


## 200 km sample
################

# Baseline

lm1 <- felm(pres ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat200)

lm2 <- felm(sen ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat200)

lm3 <- felm(gov ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat200)

# Reweighted OLS 

lm1_reweight <- felm(pres ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, weights=1/dat200$w, data=dat200)

lm2_reweight <- felm(sen ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, weights=1/dat200$w, data=dat200)

lm3_reweight <- felm(gov ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, weights=1/dat200$w, data=dat200)

# Time-interacted controls

lm4 <- felm(pres ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear + ID| 0 | ID + stateyear, data=dat200)

lm5 <- felm(sen ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear + ID| 0 | ID + stateyear, data=dat200)

lm6 <- felm(gov ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear + ID| 0 | ID + stateyear, data=dat200)

# Pure tx/control

lm7 <- felm(pres ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat200[(dat200$overlap %in% c(0,1)),])

lm8 <- felm(sen ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat200[(dat200$overlap %in% c(0,1)),])

lm9 <- felm(gov ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat200[(dat200$overlap %in% c(0,1)),])

stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"))

## 100 KM SAMPLE 
#########################

# Baseline

lm1 <- felm(pres ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat100)

lm2 <- felm(sen ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat100)

lm3 <- felm(gov ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat100)

# Reweighted OLS 

lm1_reweight <- felm(pres ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, weights=1/dat100$w, data=dat100)

lm2_reweight <- felm(sen ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, weights=1/dat100$w, data=dat100)

lm3_reweight <- felm(gov ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, weights=1/dat100$w, data=dat100)

# Time-interacted controls

lm4 <- felm(pres ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear + ID| 0 | ID + stateyear, data=dat100)

lm5 <- felm(sen ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear + ID| 0 | ID + stateyear, data=dat100)

lm6 <- felm(gov ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear + ID| 0 | ID + stateyear, data=dat100)

# Pure tx/control

lm7 <- felm(pres ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat100[(dat100$overlap %in% c(0,1)),])

lm8 <- felm(sen ~ overlap + I(overlap*post) | stateyear + ID| 0 | ID + stateyear, data=dat100[(dat100$overlap %in% c(0,1)),])

lm9 <- felm(gov ~ overlap + I(overlap*post) | stateyear + ID | 0 | ID + stateyear, data=dat100[(dat100$overlap %in% c(0,1)),])

stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"))

## Table A8: Impact of Technological Shock on Conservative Voting (2000-2020 Endline)
######################################################

load(paste0(path, "output_files/alt_dat.Rdata"))
load(paste0(path, "output_files/alt_dat200.Rdata"))
load(paste0(path, "output_files/alt_dat100.Rdata"))

# Baseline 

lm1 <- felm(pres_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat)

lm2 <- felm(sen_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat)

lm3 <- felm(gov_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat)

# Reweighted OLS 

lm1_reweight <- felm(pres_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=dat$w, data=alt_dat)

lm2_reweight <- felm(sen_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=dat$w, data=alt_dat)

lm3_reweight <- felm(gov_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=dat$w, data=alt_dat)

# Time-interacted controls

lm4 <- felm(pres_alt ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post) + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=alt_dat)

lm5 <- felm(sen_alt ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=alt_dat)

lm6 <- felm(gov_alt ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=alt_dat)

# Stable counties + buffer

lm7 <- felm(pres_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat[(alt_dat$overlap %in% c(0,1)) ,])

lm8 <- felm(sen_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat[(alt_dat$overlap %in% c(0,1)),])

lm9 <- felm(gov_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat[(alt_dat$overlap %in% c(0,1)),])

stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"))

## 200 km sample
################

# Baseline

lm1 <- felm(pres_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat200)

lm2 <- felm(sen_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat200)

lm3 <- felm(gov_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat200)

# Reweighted OLS 

lm1_reweight <- felm(pres_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=alt_dat200$w, data=alt_dat200)

lm2_reweight <- felm(sen_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=alt_dat200$w, data=alt_dat200)

lm3_reweight <- felm(gov_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=alt_dat200$w, data=alt_dat200)

# Time-interacted controls

lm4 <- felm(pres_alt ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=alt_dat200)

lm5 <- felm(sen_alt ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=alt_dat200)

lm6 <- felm(gov_alt ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=alt_dat200)

# Stable counties + buffer

lm7 <- felm(pres_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat200[alt_dat200$overlap %in% c(0,1),])

lm8 <- felm(sen_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat200[alt_dat200$overlap %in% c(0,1),])

lm9 <- felm(gov_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat200[alt_dat200$overlap %in% c(0,1),])

stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"))

## 100 km sample 
#########################

# Baseline

lm1 <- felm(pres_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat100)

lm2 <- felm(sen_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat100)

lm3 <- felm(gov_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat100)

# Reweighted OLS 

lm1_reweight <- felm(pres_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=alt_dat100$w, data=alt_dat100)

lm2_reweight <- felm(sen_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=alt_dat100$w, data=alt_dat100)

lm3_reweight <- felm(gov_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, weights=alt_dat100$w, data=alt_dat100)

# Time-interacted controls

lm4 <- felm(pres_alt ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=alt_dat100)

lm5 <- felm(sen_alt ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=alt_dat100)

lm6 <- felm(gov_alt ~ overlap + I(overlap*post) + baseline_erosion + I(baseline_erosion*post) + new_deal + I(new_deal*post)  + migrate + I(migrate*post) + drought + I(drought*post) + white + I(white*post) + WW2 + I(WW2*post)| stateyear | 0 | ID + stateyear, data=alt_dat100)

# Stable counties + buffer

lm7 <- felm(pres_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat100[alt_dat100$overlap %in% c(0,1),])

lm8 <- felm(sen_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat100[alt_dat100$overlap %in% c(0,1),])

lm9 <- felm(gov_alt ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=alt_dat100[alt_dat100$overlap %in% c(0,1),])

stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"))


## Figure A9: Pre- and Post-shock Trends Controlling for County-specific Linear Trends
#######################################################

load(paste0(path, "output_files/pres_data.Rdata"))
load(paste0(path, "output_files/sen_data.Rdata"))
load(paste0(path, "output_files/gov_data.Rdata"))

par(mfrow=c(3, 1))

## President

lm1 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) + as.factor(fips.x):election_year | election_id + fips.x | 0 | election_id + fips.x, 
			data=pres_data[pres_data$dist <100 & pres_data$fips.x%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]), ylim=c(-sd(pres_data$conservative, na.rm=T),sd(pres_data$conservative, na.rm=T)), main="A. Presidential Elections",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s")
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)[1:8]+1.96*diag(sqrt(vcov(lm1)))[1:8],
		y1=coefficients(lm1)[1:8]-1.96*diag(sqrt(vcov(lm1)))[1:8])
abline(h=0, lty=2)

## Senate

lm1 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990))  + as.factor(fips.x):election_year | election_id + fips.x | 0 | election_id + fips.x, 
			data=sen_data[sen_data$dist < 100 & sen_data$fips.x%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]), ylim=c(-sd(sen_data$conservative, na.rm=T),sd(sen_data$conservative, na.rm=T)), main="B. Senatorial Elections",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s")
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)[1:8]+1.96*diag(sqrt(vcov(lm1)))[1:8],
		y1=coefficients(lm1)[1:8]-1.96*diag(sqrt(vcov(lm1)))[1:8])
abline(h=0, lty=2)

## Governor

lm1 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990))  + as.factor(fips.x):election_year | election_id + fips.x | 0 | election_id + fips.x, 
			data=gov_data[gov_data$dist < 100 & gov_data$fips.x%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]), ylim=c(-sd(gov_data$conservative, na.rm=T),sd(gov_data$conservative, na.rm=T)), main="C. Gubernatorial Elections",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s")
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)[1:8]+1.96*diag(sqrt(vcov(lm1)))[1:8],
		y1=coefficients(lm1)[1:8]-1.96*diag(sqrt(vcov(lm1)))[1:8])
abline(h=0, lty=2)


## Table A9: Partisan Bias in Campaign Contributions
####################################################

load(paste0(path,"output_files/cf_scores.RData"))

lm1 <- felm(contributor.cfscore ~ overlap | STATENAM | 0 | 0, data=counties@data)

lm2 <- felm(contributor.cfscore ~ overlap | STATENAM | 0 | 0, data=counties@data[counties@data$dist < 200,])

lm3 <- felm(contributor.cfscore ~ overlap | STATENAM | 0 | 0, data=counties@data[counties@data$dist < 100,])


lm4 <- felm(weighted_contributor.cfscore ~ overlap | STATENAM | 0 | 0, data=counties@data)

lm5 <- felm(weighted_contributor.cfscore ~ overlap | STATENAM | 0 | 0, data=counties@data[counties@data$dist < 200,])

lm6 <- felm(weighted_contributor.cfscore ~ overlap | STATENAM | 0 | 0, data=counties@data[counties@data$dist < 100,])


stargazer(lm1, lm2, lm3, lm4, lm5, lm6)



