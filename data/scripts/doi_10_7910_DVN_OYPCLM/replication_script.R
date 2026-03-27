## Replication script for "Explaining Rural Conservatism: Political Consequences of Technological Change in the Great Plains"
## Corresponding author: Aditya Dasgupta (adasgupta3@ucmerced.edu)

## Notes: All required files attached in enclosed folder. See article text for data description.
## Once appropriate path to working directory is assigned, this R script should run in its entirety to produce all of the main figures
## and tables in the paper. All required packages listed below and may need to be installed. Please run script in its entirety
## as some of the downstream/later analyses depends on objects produced during earlier analyses. 

## Working directory
####################

path <- "YOUR WORKING DIRECTORY HERE" # Note: make sure to append slash '/' at end of WD path

## Libraries
############

library(rgdal)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(lfe)
library(stargazer)
library(magick)
library(raster)
library(rgeos)
library(rgdal)
library(geosphere)
library(RColorBrewer)
library(maptools)

##############
## Figure 1 ##
##############

load(paste0(path,"fig1_1930.RData"))
load(paste0(path,"fig1_1990.RData"))
load(paste0(path,"states.RData"))

jpeg(file=paste0(path,"fig1.jpg"), width=1400, height=1050)

# Set up layout for two plots side by side
par(mfrow=c(1,2), cex.lab=2, cex.main=2, cex.axis=2)

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
    image_write(paste0(path, "fig2.jpg"), density = "600x600")


###############################################
## Figure 3: Center-pivot irrigation diagram ##
###############################################

## Panel A: Diagram in Latex
##########

# See diagram in text

## Panel B: Haskell County Predictions
##########

## Load files

load(paste0(path,"ndvi_crop.Rdata"))
load(paste0(path,"plss.Rdata"))
load(paste0(path,"plss.Rdata"))
load(paste0(path,"counties90.Rdata"))
load(paste0(path,"aquifer.Rdata"))
load(paste0(path,"big_map.Rdata"))

## Make plot

counties <- counties90[counties90@data$STATENAM=="Kansas",]
locations <- gCentroid(plss1, byid=T)
haskell <- extent(counties[counties@data$NHGISNAM=="Haskell",])
seed <- which(locations$x > haskell[1] & locations$x < haskell[2] &
	locations$y > haskell[3] & locations$y < haskell[4])

# Open a new JPEG file
jpeg(file=paste0(path,"fig3b.jpg"), width=3600, height=3600, res=600)

# Plot without a legend
plot(ndvi.crop)
plot(plss1[seed,], add=T, border="lightblue", lwd=2)
text(locations$x[seed], locations$y[seed], round(plss1@data$year16[seed], digits=0), cex=.5)

# Close the JPEG file
dev.off()

## Panel C:
##########

plss1@data$color <- ifelse(round(plss1@data$year16)==0, "white",
	ifelse(round(plss1@data$year16)==1, "lightgreen",
	ifelse(round(plss1@data$year16)==2, "darkgreen",
	ifelse(round(plss1@data$year16)==3, "blue",
	ifelse(round(plss1@data$year16)>=4, "darkblue","white")))))

state_kansas <- unionSpatialPolygons(counties, counties@data$STATENAM)
state_line <- rgeos::intersect(aquifer, state_kansas) ## get within-state boundary segment


# Open a new JPEG file with 6x6 inch size at 600 DPI
jpeg(file=paste0(path,"fig3c.jpg"), width=4200, height=3600, res=600)

# Plotting commands
plot(plss1, col=plss1@data$color, border=F)
legend("bottom", c("0","1","2","3","4+"), fill=c("white","lightgreen","darkgreen","blue","darkblue"), inset=c(0, 0), cex=.8, horiz=TRUE, x.intersp=0.5)
plot(counties, add=T)
plot(state_line, add=T, border="lightblue", lwd=3)

# Close the JPEG file
dev.off()

## Panel D:
##########

# Open a new JPEG file with 6x6 inch size at 600 DPI
jpeg(file=paste0(path,"fig3d.jpg"), width=4200, height=3600, res=600)

# Your existing plotting code
kansas_map <- big_map[big_map@data$STATENAM=="Kansas",]
mini_dat <- subset(kansas_map@data, select=c("year1","year2","year3",
    "year4","year5","year6","year7","year8","year9",
    "year10","year11","year12","year13","year14",
    "year15","year16"))
var <- apply(mini_dat, 1, mean, na.rm=T)
pal <- brewer.pal(4, "YlGnBu") # Select 4 colors from the palette 
cols <- as.numeric(as.factor(cut(var, quantile(var), include.lowest=T)))
plot(kansas_map, col=pal[cols]) # Plot by quartiles of variable

# Adjust the legend to span the bottom of the image
legend("bottom", inset=c(0,0), fill=pal, c("1st","2nd","3rd","4th"), cex=.8, horiz=TRUE, x.intersp=0.5)

# Additional plots
plot(state_line, add=T, border="lightblue", lwd=3)

# Close the JPEG file
dev.off()

#######################################################
## Figure 4: Illustration of identification strategy ##
#######################################################

load(paste0(path,"fig3.RData"))
load(paste0(path,"aquifer.RData"))
load(paste0(path,"states.RData"))

jpeg(file=paste0(path,"fig4.jpg"), width=1400, height=1200)

par(mfrow=c(1,3), cex.lab=3, cex.main=3, cex.axis=3)

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

load(paste0(path,"fig5pre.RData"))
load(paste0(path,"fig5post.RData"))

pdf(file=paste0(path,"fig5.pdf"), width=12, height=9)

par(mfrow=c(1,2))

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

load(paste0(path,"dat.Rdata"))
load(paste0(path,"dat200.Rdata"))
load(paste0(path,"dat100.Rdata"))

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

stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"))


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

stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"))

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

stargazer(lm1, lm2, lm3, lm1_reweight, lm2_reweight, lm3_reweight, lm4, lm5, lm6, lm7, lm8, lm9, 
	omit=c("baseline_pres","baseline_sen","baseline_gov","baseline_erosion","new_deal","migrate","WW2","drought","white"))

#############
## TABLE 2 ##
#############

load(paste0(path,"tab2.Rdata"))

lm1 <- felm(irr2 ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat)

lm2 <- felm(irr2 ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[dat$dist <= 200,])

lm3 <- felm(irr2 ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[dat$dist <= 100,])

lm4 <- felm(center_pivot ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat)

lm5 <- felm(center_pivot ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[dat$dist <= 200,])

lm6 <- felm(center_pivot ~ overlap + I(overlap*post) | stateyear | 0 | ID + stateyear, data=dat[dat$dist <= 100,])

stargazer(lm1,lm2,lm3,lm4,lm5,lm6)

#############
## TABLE 3 ##
#############

load(paste0(path,"tab3pres.Rdata"))
load(paste0(path,"tab3sen.Rdata"))
load(paste0(path,"tab3gov.Rdata"))
load(paste0(path,"agdat.Rdata"))
load(paste0(path,"county_frame.Rdata"))

lm1 <- felm(conservative ~ 
	I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=temp[is.na(temp$dist)==F & temp$election_year >= 1910 & temp$fips.x%in%county_frame$fips[county_frame$stable],])

## President 200km

lm2 <- felm(conservative ~ 
	I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=temp[is.na(temp$dist)==F & temp$dist < 200 & temp$election_year >= 1910 & temp$fips.x%in%county_frame$fips[county_frame$stable],])

## President 100km

lm3 <- felm(conservative ~ 
	I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=temp[is.na(temp$dist)==F & temp$dist < 100 & temp$election_year >= 1910 & temp$fips.x%in%county_frame$fips[county_frame$stable],])
## Senator Full

lm4 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=temp1[is.na(temp1$dist)==F & temp1$fips.x%in%county_frame$fips[county_frame$stable],])

## Senator 200km

lm5 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=temp1[is.na(temp1$dist)==F & temp1$dist <= 200 & temp1$fips.x%in%county_frame$fips[county_frame$stable],])

## Senator 100km

lm6 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=temp1[is.na(temp1$dist)==F & temp1$dist <= 100 & temp1$fips.x%in%county_frame$fips[county_frame$stable],])

## Governor Full

lm7 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=temp2[is.na(temp2$dist)==F & temp2$fips.x%in%county_frame$fips[county_frame$stable],])

## Governor 200km

lm8 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=temp2[is.na(temp2$dist)==F & temp2$dist <= 200 & temp2$fips.x%in%county_frame$fips[county_frame$stable],])

## Senator 100km

lm9 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1930)) 
		+ I(overlap*I(buckets==1940)) + I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id | 0 | election_id + fips.x, 
			data=temp2[is.na(temp2$dist)==F & temp2$dist <= 100 & temp2$fips.x%in%county_frame$fips[county_frame$stable],])

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

stargazer(lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9, lm10, lm11, lm12)

#################################################
## Figure 6: Results broken out by time period ##
#################################################

load(paste0(path,"tab3pres.Rdata"))
load(paste0(path,"tab3sen.Rdata"))
load(paste0(path,"tab3gov.Rdata"))
load(paste0(path,"agdat.Rdata"))

pdf(file=paste0(path,"fig6.pdf"), width=9, height=9)

par(mfrow=c(2, 2))

## President

lm1 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id + fips.x | 0 | election_id + fips.x, 
			data=temp[temp$dist <100 & temp$fips.x%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]), ylim=c(-sd(temp$conservative, na.rm=T),sd(temp$conservative, na.rm=T)), main="A. Presidential Elections",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s")
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

## Senate

lm1 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id + fips.x | 0 | election_id + fips.x, 
			data=temp1[temp1$dist < 100 & temp1$fips.x%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]), ylim=c(-sd(temp1$conservative, na.rm=T),sd(temp1$conservative, na.rm=T)), main="B. Senatorial Elections",
	xlab="Decade (start year)", ylab="Coefficient Relative to 1930s")
segments(x0=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990), x1=c(1910, 1920, 1940, 1950, 1960, 1970, 1980, 1990),
	y0=coefficients(lm1)-1.96*diag(sqrt(vcov(lm1))),
		y1=coefficients(lm1)+1.96*diag(sqrt(vcov(lm1))))
abline(h=0, lty=2)

## Governor

lm1 <- felm(conservative ~ I(overlap*I(buckets==1910)) + I(overlap*I(buckets==1920)) + I(overlap*I(buckets==1940)) +
		I(overlap*I(buckets==1950)) + I(overlap*I(buckets==1960))  + I(overlap*I(buckets==1970)) +  I(overlap*I(buckets==1980)) +
			 + I(overlap*I(buckets==1990)) | election_id + fips.x | 0 | election_id + fips.x, 
			data=temp2[temp2$dist < 100 & temp2$fips.x%in%county_frame$fips[county_frame$stable],])
plot(c(1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990), c(coefficients(lm1)[1:2],0,coefficients(lm1)[3:8]), ylim=c(-sd(temp2$conservative, na.rm=T),sd(temp2$conservative, na.rm=T)), main="C. Gubernatorial Elections",
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

load(paste0(path,"agdat.Rdata"))
load(paste0(path,"popdat.Rdata"))
load(paste0(path,"religion_dat.Rdata"))
load(paste0(path,"county_frame.Rdata"))
load(paste0(path,"race.Rdata"))

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
stargazer(lm1,lm2,lm3,lm6,lm4,lm5,lm7,lm8,lm9,lm10)

##################################
## Figure 7: Potential Channels ##
##################################

load(paste0(path,"agdat.Rdata"))
load(paste0(path,"popdat.Rdata"))
load(paste0(path,"religion_dat.Rdata"))
load(paste0(path,"county_frame.Rdata"))
load(paste0(path,"race.Rdata"))

pdf(file=paste0(path,"fig7.pdf"), width=12, height=8)

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

###############################

load(paste0(path,"zip_sample.Rdata"))

jpeg(file=paste0(path,"fig8.jpg"), width=1400, height=1200)

par(mfrow=c(1,3), cex.lab=2, cex.main=2, cex.axis=2)

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

load(paste0(path,"tab5.Rdata"))

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

stargazer(lm1a,lm1b,lm1c,lm1d,lm1e,lm1f,lm1h, lm1i, lm1j)


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

stargazer(lm1a,lm1b,lm1c,lm1d,lm1e,lm1f,lm1h, lm1i, lm1j)


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

stargazer(lm1a,lm1b,lm1c,lm1d,lm1e,lm1f,lm1h, lm1i, lm1j)


## Panel C: 100km sample +
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

stargazer(lm1a,lm1b,lm1c,lm1d,lm1e,lm1f,lm1h, lm1i, lm1j)
