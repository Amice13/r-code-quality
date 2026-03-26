######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy


# Code adapted from replication code for Burke et al. (2015)
######



rm(list=ls())

library(ncdf4)
library(sf)
library(RSAGA)
library(maps)
library(raster)
library(exactextractr)
"%&%"<-function(x,y)paste(x,y,sep="")

# Set working Directory
path = "C:/Users/aharding6/GaTech Dropbox/Anthony Harding/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)


# Set input and output file names
fin = "data/input/HistoricalWeather/air.mon.mean.v501.nc"
fout = "data/output/MWv5/"
nc_var = "air"
dim.x = c(361:720,1:360)
dim.y = 1:360
start.year = 1900



################################################################################
# Step 1 - Import Data
################################################################################
# Import country shapefile
cty=st_read('data/input/shape/country.shp')  #shapefile of global countries, as provided by ESRI distribution
cty1 <- cty[cty$CNTRY_NAME!="Antarctica" & cty$CNTRY_NAME!="Svalbard",]  #drop antarctica

# Read in gridded temperature, using data from here: Matsuura, K. & Willmott, C. J. Terrestrial air temperature and precipitation: monthly and annual time series v. 5.01.
nc <- nc_open(fin)
tmp <- ncvar_get(nc,nc_var)
nc_close(nc)
# ARH - convert to annual average C, which is the units for the regression data
yrs = rep(c(1:(as.integer(dim(tmp)[3]/12)+1)),each=12)[1:dim(tmp)[3]]
tmp <- apply(tmp,c(1,2),FUN=function(x){tapply(x,yrs,mean)})
r <- brick(aperm(tmp[,dim.x,dim.y],c(3,2,1)),xmn=-180,xmx=180,ymn=-90,ymx=90) # Make raster

# plot temperatures
plot(r[,,1])
map(,add=T)

# Read in population data from Gridded Population of the World dataset
pop = read.ascii.grid("data/input/populationData/glp00ag30.asc") #check out ?SpatialGridDataFrame, which is the class of the thing that is getting read in
pop=as.matrix(pop$data) # Convert to matrix
pop=raster((pop),xmn=-180,xmx=180,ymn=-58,ymx=85) # Convert to raster

################################################################################
# End Step 1 - End Step 1 - End Step 1 - End Step 1 - End Step 1 - End Step 1 
################################################################################

  
################################################################################
# Step 2 - Calculate population weighted temperature change
################################################################################
# Make temp and pop rasters the same size and resolution
rr <- crop(r, pop)  #crop temp raster to size of population raster
pw <- pop
 

# Calculate population-weighted temperature
Tchg <- exact_extract(r,cty1,'weighted_mean',weights=pw)
colnames(Tchg) <- "temp"%&%c(start.year:(start.year+dim(r)[[3]]-1))


# Get global mean temperature
y <- init(r,v='y')
y <- cos(y*pi/180)  #cosine of latitude, converted to radians.
y <- y/cellStats(y,'sum')  #make weights sum to 1 to make weighting matrix
tc <- cellStats(y*r,'sum')  #this is the global weighted mean temperature change
names(tc) <- as.character(c(start.year:(start.year+dim(r)[[3]]-1)))

# Store results in a dataframe
out <- data.frame(st_drop_geometry(cty1[,1:3]),Tchg)

# Export temperature change data
write.csv(out,file=paste0(fout,"CountryTemp.csv"),row.names=F)
save(tc,file=paste0(fout,"GlobalTemp.dta"))

################################################################################
# End Step 2 - End Step 2 - End Step 2 - End Step 2 - End Step 2 - End Step 2 
################################################################################

