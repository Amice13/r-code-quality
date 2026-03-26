######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy


# CODE TO CALCULATE COUNTRY-SPECIFIC CHANGE IN PRECIPITATION UNDER RCP8.5, AND UNDER DIFFERENT GLOBAL AVERAGE WARMINGS (SO WE CAN CALCULATE DAMAGE FUNCTION)
# We are using CMIP5 RCP8.5 ensemble mean data from here: http://climexp.knmi.nl/plot_atlas_form.py 
# Code adapted from replication code for Burke et al. (2015)
######



rm(list=ls())

library(ncdf4)
library(maptools)
library(maps)
library(raster)
"%&%"<-function(x,y)paste(x,y,sep="")

# Set working Directory
path = "C:/Users/aharding6/Dropbox (GaTech)/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)



################################################################################
# Step 1 - Import Data
################################################################################
# Import country shapefile
cty=readShapePoly('data/input/shape/country.shp')  #shapefile of global countries, as provided by ESRI distribution
cty1 <- cty[cty@data[,3]!="Antarctica" & cty@data[,3]!="Svalbard",]  #drop antarctica

# Read in CMIP5 global precipitation projections, using data from here: http://climexp.knmi.nl/plot_atlas_form.py 
#  these are model ensemble averages, giving precipitation changes 2080-2100 minus 1986-2005
nc <- nc_open("data/input/CCprojections/diff_pr_Amon_modmean_rcp85_000_2081-2100_minus_1986-2005_mon1_ave12_withsd.nc") #Units are mm/day
pcp <- ncvar_get(nc,"diff") # Units mm/day
pcp <- pcp*365 # ARH - convert to mm/year, which is the units for the Udel_precip data
r <- raster(aperm(pcp[c(73:144,1:72),72:1],c(2,1)),xmn=-180,xmx=180,ymn=-90,ymx=90)


# plot precipitations
plot(r)
map(,add=T)

# Read in population data from Gridded Population of the World dataset
pop = readAsciiGrid("data/input/populationData/glp00ag30.asc") #check out ?SpatialGridDataFrame, which is the class of the thing that is getting read in
pop=as.matrix(pop) # Convert to matrix
pop=raster(t(pop),xmn=-180,xmx=180,ymn=-58,ymx=85) # Convert to raster

################################################################################
# End Step 1 - End Step 1 - End Step 1 - End Step 1 - End Step 1 - End Step 1 
################################################################################


################################################################################
# Step 2 - Calculate population weighted precipitation change
################################################################################
# Make temp and pop rasters the same size and resolution
rr <- crop(r, pop)  #crop temp raster to size of population raster
pw <- aggregate(pop,fact=5)  #aggregate population raster up to 2.5deg, the resolution of the downloaded GCM data

# Extract precipitation and population for each country
cc <- extract(r,cty1,small=T,progress="text")  # returns a list where each element is the cell numbers in the pop raster that are covered by a given country.  
pp <- extract(pw,cty1,small=T,progress="text")

# Calculated population-weighted precipitation change
wtmn <- function(x,y) {if (length(x)>1 & sum(y)!=0) {weighted.mean(x,y)} else {mean(x)}}  # if country only covered by one cell, or if no population in aggregated grids, just report value of delta T in that cell
Pchg <- mapply(wtmn,cc,pp)  #this gives you the country-specific population-weighted temperature change


# Now calculate a vector of "conversion factors" that translate global mean temp into country specific temperatures:  this is just the ratio of pop-weighted country-specific changes to global mean change in RCP8.5
# This then allows us to calculate damages for various levels of warming
y <- init(r,v='y')
y <- cos(y*pi/180)  #cosine of latitude, converted to radians.  
y <- y/cellStats(y,'sum')  #make weights sum to 1 to make weighting matrix
pc <- cellStats(y*r,'sum')  #this is the global weighted mean temperature change
Pconv <- Pchg/pc  #"conversion factors":  i.e. what you multiply the global mean temp by to get the country-level change in temp.  again, this is based only on RCP8.5 ensemble mean 

# Store results in a dataframe
out <- data.frame(cty1@data[,1:3],Pchg,Pconv)

# Export temperature change data
write.csv(out,file="data/input/CCprojections/CountryPrecipChange_RCP85.csv",row.names=F)
# ARH - Export global mean precip change
write.csv(pc,file="data/input/CCprojections/GlobalPrecipChange_RCP85.csv",row.names=F)

################################################################################
# End Step 2 - End Step 2 - End Step 2 - End Step 2 - End Step 2 - End Step 2 
################################################################################
