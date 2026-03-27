#Project #1 NOAA Data

load("predictors.Rdata")
library(fields)

# dates:        all dates for which data are available
# stations:     names of all stations for which data are available
# lat, lon:     longitude and latitude coordinate for each of these stations
# elev:         station elevation (m)
# ptype:        all cases where one of the four precipitation types of interest was reported
				#Types are RA = Rain, SN = Snow, FZRA = Freezing Rain, IP = Ice Pellets
# Twb.prof:     the corresponding vertical profiles of wetbulb temperature (0m to 3000m above the surface in steps of 100m)
# station.ind:  for each case, the index of the station where it was reported
# date.ind:     for each case, the index of the date on which it was reported


ls()
head(date.ind)
head(dates)
head(elev)
head(lat)
head(lon)
head(ptype)
head(station.ind)
head(stations)
head(Twb.prof)

dim(Twb.prof)


######################
#Plot the stations
######################

lon.new <- lon-360

plot(lon.new,lat,pch=19)
map("world",add=T)
map("state",add=T)

######################
#Get month and year
######################

year<-as.numeric(substr(dates, 1, 4))
unique(year)

month<-as.numeric(substr(dates, 5, 6))
sort(unique(month))

######################
#Proportion of observations of each type
######################


head(ptype)
summary(ptype)
tail(ptype)
unique(ptype)
class(ptype)
table(ptype)

nn <- length(ptype)
table(ptype)/nn*100

p1 <- length(which(ptype == "RA"))/nn
p2 <- length(which(ptype == "SN"))/nn
p3 <- length(which(ptype == "FZRA"))/nn
p4 <- length(which(ptype == "IP"))/nn

p1+p2+p3+p4
nn
p1*100
p2*100
p3*100
p4*100

######################
#Plotting some of the temperature vertical profiles
######################

dim(Twb.prof)
head(Twb.prof)


step.size <- seq(0, 3000, by = 100)
xlims <- range(Twb.prof[1:100, ])


#First vertical profile
plot(Twb.prof[1,], step.size, xlab="Temperature (K)", ylab="Meters AGL", type="l")

#First 100 vertical profiles
plot(Twb.prof[1,], step.size, xlab="Temperature (K)", ylab="Meters AGL", type="l", xlim=xlims)
for(i in 1:100){
	lines(Twb.prof[i,],step.size)
}

abline(v = 273.15, col = 2, lwd = 2)


#Ice Pellet Profiles
pellet <- which(ptype == "IP")
xlims <- range(Twb.prof[pellet,])
plot(Twb.prof[1,], step.size, xlab = "Temperature (K)", ylab = "Meters AGL", type = "n", xlim = xlims, col = rgb(0,0,0,.1))
title("Ice Pellet Profiles",cex.main = 2)
abline(v = 273.15, col = 2, lwd = 2)
for(i in 1:length(pellet)){
	lines(Twb.prof[pellet[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=5,lwd=3)
#lines(Twb.prof[pellet[694],],step.size,col=4,lwd=2)
#lines(Twb.prof[pellet[574],],step.size,col=2,lwd=2)
#lines(Twb.prof[pellet[696],],step.size,col=2,lwd=2)


#Rain Profiles
rain <- which(ptype=="RA")
xlims <- range(Twb.prof[rain,])
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Rain Profiles",cex.main=2)
abline(v=273.15,col=2,lwd=2)
for(i in 1:length(rain)){
	lines(Twb.prof[rain[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=2,lwd=2)


#Snow Profiles
snow <- which(ptype=="SN")
xlims <- range(Twb.prof[snow,])
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Snow Profiles",cex.main=2)
abline(v=273.15,col=2,lwd=2)
for(i in 1:length(snow)){
	lines(Twb.prof[snow[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=2,lwd=2)



#Freezing Rain Profiles
frza <- which(ptype=="FZRA")
xlims <- range(Twb.prof[frza,])
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Freezing Rain Profiles",cex.main=2)
abline(v=273.15,col=2,lwd=2)
for(i in 1:length(frza)){
	lines(Twb.prof[frza[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=2,lwd=2)



